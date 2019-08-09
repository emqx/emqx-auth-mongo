%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-include_lib("emqx/include/emqx.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(APP, emqx_auth_mongo).

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
    [{emqx_auth_mongo_auth, [sequence], [check_auth]},
     {emqx_auth_mongo_acl, [sequence], [check_acl, acl_super]},
     {auth_mongo_config, [sequence], [server_config]}].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_auth_mongo], fun set_special_configs/1),
    {ok, Connection} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, emqx_auth_mongo})),
    [{connection, Connection} | Config].

end_per_suite(Config) ->
    {ok, Connection} = ?POOL(?APP),
    AuthCollection = collection(authquery, Config),
    AclCollection = collection(aclquery, Config),
    mongo_api:delete(Connection, AuthCollection, {}),
    mongo_api:delete(Connection, AclCollection, {}),
    application:stop(emqx_auth_mongo),
    application:stop(emqx).

set_special_configs(emqx) ->
    application:set_env(emqx, acl_nomatch, deny),
    application:set_env(emqx, acl_file,
                        emqx_ct_helpers:deps_path(emqx, "test/emqx_SUITE_data/acl.conf")),
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                        emqx_ct_helpers:deps_path(emqx, "test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(_App) ->
    ok.

check_auth(_Config) ->
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(emqx_auth_mongo, auth_query),
    Collection = collection(authquery, AppConfig),
    mongo_api:delete(Connection, Collection, {}),
    InitR = mongo_api:insert(Connection, Collection, ?INIT_AUTH),
    ct:pal("init auth result: ~p~n", [InitR]),

    Plain = #{zone => external, client_id => <<"client1">>, username => <<"plain">>},
    Plain1 = #{zone => external, client_id => <<"client1">>, username => <<"plain2">>},
    Md5 = #{zone => external, client_id => <<"md5">>, username => <<"md5">>},
    Sha = #{zone => external, client_id => <<"sha">>, username => <<"sha">>},
    Sha256 = #{zone => external, client_id => <<"sha256">>, username => <<"sha256">>},
    Pbkdf2 = #{zone => external, client_id => <<"pbkdf2_password">>, username => <<"pbkdf2_password">>},
    Bcrypt = #{zone => external, client_id => <<"bcrypt_foo">>, username => <<"bcrypt_foo">>},
    User1 = #{zone => external, client_id => <<"bcrypt_foo">>, username => <<"user">>},
    reload({auth_query, [{password_hash, plain}]}),
    %% With exactly username/password, connection success
    {ok, #{is_superuser := true}} = emqx_access_control:authenticate(Plain#{password => <<"plain">>}),
    %% With exactly username and wrong password, connection fail
    {error, _} = emqx_access_control:authenticate(Plain#{password => <<"error_pwd">>}),
    %% With wrong username and wrong password, emqx_auth_mongo auth fail, then allow anonymous authentication
    {error, _} = emqx_access_control:authenticate(Plain1#{password => <<"error_pwd">>}),
    %% With wrong username and exactly password, emqx_auth_mongo auth fail, then allow anonymous authentication
    {error, _} = emqx_access_control:authenticate(Plain1#{password => <<"plain">>}),
    reload({auth_query, [{password_hash, md5}]}),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Md5#{password => <<"md5">>}),
    reload({auth_query, [{password_hash, sha}]}),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha#{password => <<"sha">>}),
    reload({auth_query, [{password_hash, sha256}]}),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha256#{password => <<"sha256">>}),
    %%pbkdf2 sha
    reload({auth_query, [{password_hash, {pbkdf2, sha, 1, 16}}, {password_field, [<<"password">>, <<"salt">>]}]}),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Pbkdf2#{password => <<"password">>}),
    reload({auth_query, [{password_hash, {salt, bcrypt}}]}),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Bcrypt#{password => <<"foo">>}),
    {error, _} = emqx_access_control:authenticate(User1#{password => <<"foo">>}).

check_acl(_Config) ->
    ct:pal("acl cache enabled: ~p~n", [application:get_env(emqx, enable_acl_cache)]),
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(?APP, acl_query),
    Collection = collection(aclquery, AppConfig),
    mongo_api:delete(Connection, Collection, {}),
    InitR = mongo_api:insert(Connection, Collection, ?INIT_ACL),
    ct:pal("init acl result: ~p~n", [InitR]),
    User1 = #{zone => external, client_id => <<"client1">>, username => <<"testuser">>},
    User2 = #{zone => external, client_id => <<"client2">>, username => <<"dashboard">>},
    User3 = #{zone => external, client_id => <<"client2">>, username => <<"user3">>},
    User4 = #{zone => external, client_id => <<"$$client2">>, username => <<"$$user3">>},
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
    reload({auth_query, [{password_hash, plain}, {password_field, [<<"password">>]}]}),
    {ok, C} = emqx_client:start_link([{host, "localhost"},
                                      {client_id, <<"simpleClient">>},
                                      {username, <<"plain">>},
                                      {password, <<"plain">>}]),
    {ok, _} = emqx_client:connect(C),
    timer:sleep(10),
    emqx_client:subscribe(C, <<"TopicA">>, qos2),
    timer:sleep(1000),
    emqx_client:publish(C, <<"TopicA">>, <<"Payload">>, qos2),
    timer:sleep(1000),
    receive
        {publish, #{payload := Payload}} ->
        ?assertEqual(<<"Payload">>, Payload)
    after
        1000 ->
        ct:fail({receive_timeout, <<"Payload">>}),
        ok
    end,
    emqx_client:disconnect(C).

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
