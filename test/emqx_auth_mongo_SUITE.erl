%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_mongo_SUITE).

-compile(export_all).

-import(proplists, [get_value/3]).

-include("emqx_auth_mongo.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(POOL(App),  ecpool_worker:client(gproc_pool:pick_worker({ecpool, App}))).

-define(INIT_ACL, [{<<"username">>, <<"testuser">>, <<"clientid">>, <<"null">>, <<"subscribe">>, [<<"#">>]},
                   {<<"username">>, <<"dashboard">>, <<"clientid">>, <<"null">>, <<"pubsub">>, [<<"$SYS/#">>]},
                   {<<"username">>, <<"user3">>, <<"clientid">>, <<"null">>, <<"publish">>, [<<"a/b/c">>]}]).

-define(INIT_AUTH, [{<<"username">>, <<"plain">>, <<"password">>, <<"plain">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, true},
                    {<<"username">>, <<"md5">>, <<"password">>, <<"1bc29b36f623ba82aaf6724fd3b16718">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, false},
                    {<<"username">>, <<"sha">>, <<"password">>, <<"d8f4590320e1343a915b6394170650a8f35d6926">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, false},
                    {<<"username">>, <<"sha256">>, <<"password">>, <<"5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, false},
                    {<<"username">>, <<"pbkdf2_password">>, <<"password">>, <<"cdedb5281bb2f801565a1122b2563515">>, <<"salt">>, <<"ATHENA.MIT.EDUraeburn">>, <<"is_superuser">>, false},
                    {<<"username">>, <<"bcrypt_foo">>, <<"password">>, <<"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6">>, <<"salt">>, <<"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.">>, <<"is_superuser">>, false}
                    ]).

all() ->
    [{group, emqx_auth_mongo_auth},
     {group, emqx_auth_mongo_acl},
     {group, auth_mongo_config}].

groups() ->
    [{emqx_auth_mongo_auth, [sequence], [check_auth, list_auth]},
     {emqx_auth_mongo_acl, [sequence], [check_acl, acl_super]},
     {auth_mongo_config, [sequence], [server_config]}].

init_per_suite(Config) ->
    [run_setup_steps(App) || App <- [emqx, emqx_auth_mongo]],
    {ok, Connection} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, emqx_auth_mongo})),
    [{connection, Connection} | Config].

end_per_suite(Config) ->
    {ok, Connection} = ?POOL(?APP),
    AuthCollection = collection(authquery, Config),
    AclCollection = collection(aclquery, Config),
    mongo_api:delete(Connection, AuthCollection, {}),
    mongo_api:delete(Connection, AclCollection, {}),
    application:stop(emqx_auth_mongo),
    application:stop(emqttd).

check_auth(Config) ->
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(emqx_auth_mongo, auth_query),
    Collection = collection(authquery, AppConfig),
    mongo_api:delete(Connection, Collection, {}),
    mongo_api:insert(Connection, Collection, ?INIT_AUTH),

    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    Plain1 = #{client_id => <<"client1">>, username => <<"plain2">>},
    Md5 = #{client_id => <<"md5">>, username => <<"md5">>},
    Sha = #{client_id => <<"sha">>, username => <<"sha">>},
    Sha256 = #{client_id => <<"sha256">>, username => <<"sha256">>},
    Pbkdf2 = #{client_id => <<"pbkdf2_password">>, username => <<"pbkdf2_password">>},
    Bcrypt = #{client_id => <<"bcrypt_foo">>, username => <<"bcrypt_foo">>},
    User1 = #{client_id => <<"bcrypt_foo">>, username => <<"user">>},
    reload({auth_query, [{password_hash, plain}]}),
    %% With exactly username/password, connection success
    {ok, true} = emqx_access_control:authenticate(Plain, <<"plain">>),
    %% With exactly username and wrong password, connection fail
    {error, password_error} = emqx_access_control:authenticate(Plain, <<"error_pwd">>),
    %% With wrong username and wrong password, emqx_auth_mongo auth fail, then allow anonymous authentication
    ok = emqx_access_control:authenticate(Plain1, <<"error_pwd">>),
    %% With wrong username and exactly password, emqx_auth_mongo auth fail, then allow anonymous authentication
    ok = emqx_access_control:authenticate(Plain1, <<"plain">>),
    reload({auth_query, [{password_hash, md5}]}),
    {ok, false} = emqx_access_control:authenticate(Md5, <<"md5">>),
    reload({auth_query, [{password_hash, sha}]}),
    {ok, false} = emqx_access_control:authenticate(Sha, <<"sha">>),
    reload({auth_query, [{password_hash, sha256}]}),
    {ok, false} = emqx_access_control:authenticate(Sha256, <<"sha256">>),
    %%pbkdf2 sha
    reload({auth_query, [{password_hash, {pbkdf2, sha, 1, 16}}, {password_field, [<<"password">>, <<"salt">>]}]}),
    {ok, false} = emqx_access_control:authenticate(Pbkdf2, <<"password">>),
    reload({auth_query, [{password_hash, {salt, bcrypt}}]}),
    {ok, false} = emqx_access_control:authenticate(Bcrypt, <<"foo">>),
    ok = emqx_access_control:authenticate(User1, <<"foo">>).

list_auth(_Config) ->
    application:start(emqx_auth_username),
    emqx_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #{client_id => <<"client1">>, username => <<"user1">>},
    ok = emqx_access_control:authenticate(User1, <<"password1">>),
    reload({auth_query, [{password_hash, plain}, {password_field, [<<"password">>]}]}),
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    {ok, true} = emqx_access_control:authenticate(Plain, <<"plain">>),
    application:stop(emqx_auth_username).

check_acl(Config) ->
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(?APP, acl_query),
    Collection = collection(aclquery, AppConfig),
    mongo_api:delete(Connection, Collection, {}),
    mongo_api:insert(Connection, Collection, ?INIT_ACL),
    User1 = #{client_id => <<"client1">>, username => <<"testuser">>},
    User2 = #{client_id => <<"client2">>, username => <<"dashboard">>},
    User3 = #{client_id => <<"client2">>, username => <<"user3">>},
    User4 = #{client_id => <<"$$client2">>, username => <<"$$user3">>},
    3 = mongo_api:count(Connection, Collection, {}, 17),
    %% ct log output
    %%ct_log(Connection, Collection, User1),
    allow = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    deny = emqx_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
    allow = emqx_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>),
    allow = emqx_access_control:check_acl(User3, publish, <<"a/b/c">>),
    deny = emqx_access_control:check_acl(User3, publish, <<"c">>),
    allow = emqx_access_control:check_acl(User4, publish, <<"a/b/c">>).

acl_super(_Config) ->
    reload({auth_query, [{password_hash, plain}]}),
    {ok, C} = emqttc:start_link([{host, "localhost"},
                                 {client_id, <<"simpleClient">>},
                                 {username, <<"plain">>},
                                 {password, <<"plain">>}]),
    timer:sleep(10),
    emqttc:subscribe(C, <<"TopicA">>, qos2),
    timer:sleep(1000),
    emqttc:publish(C, <<"TopicA">>, <<"Payload">>, qos2),
    timer:sleep(1000),
    receive
        {publish, Topic, Payload} ->
        ?assertEqual(<<"Payload">>, Payload)
    after
        1000 ->
        io:format("Error: receive timeout!~n"),
        ok
    end,
    emqttc:disconnect(C).

collection(Query, Config) ->
    iolist_to_binary(case Query of
                         superquery ->
                             get_value(collection, Config, "mqtt_user");
                         authquery ->
                             get_value(collection, Config, "mqtt_user");
                         aclquery ->
                             get_value(collection, Config, "mqtt_acl")
                     end).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_query, auth_query]],
    application:start(?APP),
    ?assertEqual([], emqx_access_control:lookup_mods(auth)),
    ?assertEqual([], emqx_access_control:lookup_mods(acl)).

server_config(_) ->
    Server =
          [{type,unknown},
           {hosts,["localhost:6377"]},
           {options,[{pool_size,1},{max_overflow,0}]},
           {worker_options,
               [{database,<<"mqtt">>},
                {auth_source,<<"mqtt">>}]},
           {auto_reconnect,1},
           {pool_size,1}],
    Auth_query =
          [{collection,"mqtt_usertest"},
           {password_field,[<<"password1">>]},
           {password_hash,{sha256,salt}},
           {selector,"username=%c"}],
    Super_query =
          [{collection,"mqtt_usertest"},
           {super_field,"is_superuser11"},
           {selector,"username=%c"}],

    Acl_query = [{collection,"mqtt_acltest"},{selector,"username=%c"}],
    SetConfigKeys = ["server=localhost:6377",
                     "type=unknown",
                     "pool=1",
                     "login=admin",
                     "password=public",
                     "database=mqtt",
                     "auth_query.collection=mqtt_usertest",
                     "auth_query.password_field=password1",
                     "auth_query.password_hash=sha256,salt",
                     "auth_query.selector=username=%c",
                     "super_query.collection=mqtt_usertest",
                     "super_query.super_field=is_superuser11",
                     "super_query.selector=username=%c",
                     "acl_query.collection=mqtt_acltest",
                     "acl_query.selector=username=%c"],

    lists:foreach(fun set_cmd/1, SetConfigKeys),
    {ok, S} =  application:get_env(emqx_auth_mongo, server),
    {ok, A} =  application:get_env(emqx_auth_mongo, auth_query),
    {ok, Super} =  application:get_env(emqx_auth_mongo, super_query),
    {ok, Acl} =  application:get_env(emqx_auth_mongo, acl_query),
    ?assertEqual(lists:sort(Server), lists:sort(S)),
    ?assertEqual(lists:sort(Auth_query), lists:sort(A)),
    ?assertEqual(lists:sort(Super_query), lists:sort(Super)),
    ?assertEqual(lists:sort(Acl_query), lists:sort(Acl)).

set_cmd(Key) ->
    clique:run(["config", "set", string:join(["auth.mongo", Key], "."), "--app=emqx_auth_mongo"]).

ct_log(Connection, Collection, User1) ->
    Selector = {list_to_binary("username"), list_to_binary("%u")},
    find(Connection, Collection, emqx_auth_mongo:replvar(Selector, User1)).
   % ct:log("Got:~p", [Res]).

%% @private
find(Connection, Collection, Selector) ->
    find(Connection, Collection, Selector, #{}).

find(Connection, Collection, Selector, Projector) ->
    Cursor = mongo_api:find(Connection, Collection, Selector, #{projector => Projector}),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.

run_setup_steps(App) ->
    NewConfig = generate_config(App),
    lists:foreach(fun set_app_env/1, NewConfig),
    application:ensure_all_started(App).

generate_config(emqx) ->
    Schema = cuttlefish_schema:files([local_path(["deps","emqx", "priv", "emqx.schema"])]),
    Conf = conf_parse:file([local_path(["deps", "emqx","etc", "emqx.conf"])]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(emqx_auth_mongo) ->
    Schema = cuttlefish_schema:files([local_path(["priv", "emqx_auth_mongo.schema"])]),
    Conf = conf_parse:file([local_path(["etc", "emqx_auth_mongo.conf"])]),
    cuttlefish_generator:map(Schema, Conf).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

get_base_dir() ->
    get_base_dir(?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).

set_app_env({App, Lists}) ->
    lists:foreach(fun({acl_file, _Var}) ->
                        application:set_env(App, acl_file, local_path(["deps", "emqx", "etc", "acl.conf"]));
                     ({license_file, _Var}) ->
                        application:set_env(App, license_file, local_path(["deps", "emqx", "etc", "emqx.lic"]));
                     ({plugins_loaded_file, _Var}) ->
                        application:set_env(App, plugins_loaded_file, local_path(["deps","emqx","test", "emqx_SUITE_data","loaded_plugins"]));
                     ({Par, Var}) ->
                        application:set_env(App, Par, Var)
                  end, Lists).

reload({Par, Vals}) when is_list(Vals) ->
    application:stop(?APP),
    {ok, TupleVals} = application:get_env(?APP, Par),
    NewVals =
    lists:filtermap(fun({K, V}) ->
        case lists:keymember(K, 1, Vals) of
        false ->{true, {K, V}};
        _ -> false
        end
    end, TupleVals),
    application:set_env(?APP, Par, lists:append(NewVals, Vals)),
    application:start(?APP).

