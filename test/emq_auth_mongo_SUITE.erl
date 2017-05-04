%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 Feng Lee <feng@emqtt.io>.
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
%%--------------------------------------------------------------------

-module(emq_auth_mongo_SUITE).

-compile(export_all).

-import(proplists, [get_value/3]).

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-include("emq_auth_mongo.hrl").

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
    [{group, emq_auth_mongo_auth},
     {group, emq_auth_mongo_acl},
     {group, emq_auth_mongo}].

groups() -> 
    [{emq_auth_mongo_auth, [sequence],
     [check_auth, list_auth]},
    {emq_auth_mongo_acl, [sequence],
     [check_acl, acl_super]},
    {emq_auth_mongo, [sequence],
     [comment_config]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqttd, emq_auth_mongo]],
    Config.

end_per_suite(Config) ->
    {ok, Connection} = ?POOL(?APP),
    AuthCollection = collection(authquery, Config),
    AclCollection = collection(aclquery, Config),
    mc_worker_api:delete(Connection, AuthCollection, {}),
    mc_worker_api:delete(Connection, AclCollection, {}),
    application:stop(emq_auth_mongo),
    application:stop(emqttd).

check_auth(Config) ->
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(emq_auth_mongo, auth_query),
    Collection = collection(authquery, AppConfig),
    mc_worker_api:delete(Connection, Collection, {}),
    mc_worker_api:insert(Connection, Collection, ?INIT_AUTH),

    Plain = #mqtt_client{client_id = <<"client1">>, username = <<"plain">>},
    Md5 = #mqtt_client{client_id = <<"md5">>, username = <<"md5">>},
    Sha = #mqtt_client{client_id = <<"sha">>, username = <<"sha">>},
    Sha256 = #mqtt_client{client_id = <<"sha256">>, username = <<"sha256">>},
    Pbkdf2 = #mqtt_client{client_id = <<"pbkdf2_password">>, username = <<"pbkdf2_password">>},
    Bcrypt = #mqtt_client{client_id = <<"bcrypt_foo">>, username = <<"bcrypt_foo">>},
    User1 = #mqtt_client{client_id = <<"bcrypt_foo">>, username = <<"user">>},
    reload({auth_query, [{password_hash, plain}]}),
    {ok, true} = emqttd_access_control:auth(Plain, <<"plain">>),
    reload({auth_query, [{password_hash, md5}]}),
    {ok, false} = emqttd_access_control:auth(Md5, <<"md5">>),
    reload({auth_query, [{password_hash, sha}]}),
    {ok, false} = emqttd_access_control:auth(Sha, <<"sha">>),
    reload({auth_query, [{password_hash, sha256}]}),
    {ok, false} = emqttd_access_control:auth(Sha256, <<"sha256">>),
    %%pbkdf2 sha
    reload({auth_query, [{password_hash, {pbkdf2, sha, 1, 16}}, {password_field, [<<"password">>, <<"salt">>]}]}),
    {ok, false} = emqttd_access_control:auth(Pbkdf2, <<"password">>),
    reload({auth_query, [{password_hash, {salt, bcrypt}}]}),
    {ok, false} = emqttd_access_control:auth(Bcrypt, <<"foo">>),
    ok = emqttd_access_control:auth(User1, <<"foo">>).

list_auth(_Config) ->
    application:start(emq_auth_username),
    emq_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"user1">>},
    ok = emqttd_access_control:auth(User1, <<"password1">>),
    reload({auth_query, [{password_hash, plain}, {password_field, [<<"password">>]}]}),
    Plain = #mqtt_client{client_id = <<"client1">>, username = <<"plain">>},
    {ok, true} = emqttd_access_control:auth(Plain, <<"plain">>),
    application:stop(emq_auth_username).

check_acl(Config) ->
    {ok, Connection} = ?POOL(?APP),
    {ok, AppConfig} = application:get_env(?APP, acl_query),
    Collection = collection(aclquery, AppConfig),
    mc_worker_api:delete(Connection, Collection, {}),
    mc_worker_api:insert(Connection, Collection, ?INIT_ACL),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"dashboard">>},
    User3 = #mqtt_client{client_id = <<"client2">>, username = <<"user3">>},
    User4 = #mqtt_client{client_id = <<"$$client2">>, username = <<"$$user3">>},
    3 = mc_worker_api:count(Connection, Collection, {}),
    %% ct log output
    %%ct_log(Connection, Collection, User1),
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    deny = emqttd_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
    allow = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>),
    allow = emqttd_access_control:check_acl(User3, publish, <<"a/b/c">>),
    deny = emqttd_access_control:check_acl(User3, publish, <<"c">>),
    allow = emqttd_access_control:check_acl(User4, publish, <<"a/b/c">>).

acl_super(_Config) ->
    reload({auth_query, [{password_hash, plain}]}),
    {ok, C} = emqttc:start_link([{host, "localhost"}, {client_id, <<"simpleClient">>}, {username, <<"plain">>}, {password, <<"plain">>}]),
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
    case Query of
    superquery ->
        list_to_binary(get_value(collection, Config, "mqtt_user"));
    authquery ->
        list_to_binary(get_value(collection, Config, "mqtt_user"));
    aclquery ->
        list_to_binary(get_value(collection, Config, "mqtt_acl"))
    end.

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_query, auth_query]],
    application:start(?APP),
    ?assertEqual([], emqttd_access_control:lookup_mods(auth)),
    ?assertEqual([], emqttd_access_control:lookup_mods(acl)).

ct_log(Connection, Collection, User1) ->
    Selector = {list_to_binary("username"), list_to_binary("%u")},
    find(Connection, Collection, emq_auth_mongo:replvar(Selector, User1)).
   % ct:log("Got:~p", [Res]).

%% @private
find(Connection, Collection, Selector) ->
    find(Connection, Collection, Selector, #{}).

find(Connection, Collection, Selector, Projector) ->
    Cursor = mc_worker_api:find(Connection, Collection, Selector, #{projector => Projector}),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
%%    ct:log("NewConfig:~p", [NewConfig]),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

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
