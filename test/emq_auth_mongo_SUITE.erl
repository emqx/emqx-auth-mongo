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

-include("emq_auth_mongo.hrl").

-define(INIT_ACL, [{<<"username">>, <<"testuser">>, <<"clientid">>, <<"null">>, <<"subscribe">>, [<<"#">>]},
                   {<<"username">>, <<"dashboard">>, <<"clientid">>, <<"null">>, <<"pubsub">>, [<<"$SYS/#">>]},
                   {<<"username">>, <<"user3">>, <<"clientid">>, <<"null">>, <<"publish">>, [<<"a/b/c">>]}]).

-define(INIT_AUTH, [{<<"username">>, <<"test">>, <<"password">>, <<"b95de58f7646da3b2de64466b3429244885addac">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, false},
                    {<<"username">>, <<"root">>, <<"password">>, <<"3ef26c7a285bbfdebd8ebe895dbada207d926c15">>, <<"salt">>, <<"salt">>, <<"is_superuser">>, true}]).

all() -> 
    [{group, emq_auth_mongo}].

groups() -> 
    [{emq_auth_mongo, [sequence],
     [check_acl,
      check_auth]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqttd, emq_auth_mongo]],
    {ok, Connection} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, emq_auth_mongo})),
    [{connection, Connection} | Config].

end_per_suite(_Config) ->
    application:stop(emq_auth_mongo),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(Config) ->
    Connection = proplists:get_value(connection, Config),
    {ok, AppConfig} = application:get_env(emq_auth_mongo, acl_query),
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
    allow = emqttd_access_control:check_acl(User4, publish, <<"a/b/c">>),
    mc_worker_api:delete(Connection, Collection, {}).

check_auth(Config) ->
    Connection = proplists:get_value(connection, Config),
    {ok, AppConfig} = application:get_env(emq_auth_mongo, auth_query),
    Collection = collection(authquery, AppConfig),
    mc_worker_api:delete(Connection, Collection, {}),
    mc_worker_api:insert(Connection, Collection, ?INIT_AUTH),

    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"test">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"root">>},
    User3 = #mqtt_client{client_id = <<"client3">>},
	{ok, false} = emqttd_access_control:auth(User1, <<"test">>),
    {error, _} = emqttd_access_control:auth(User1, <<"pwderror">>),
	{ok, true} = emqttd_access_control:auth(User2, <<"test1">>),
    {error, username_or_password_undefined }= emqttd_access_control:auth(User2, <<>>),
    {error, username_or_password_undefined} = emqttd_access_control:auth(User3, <<>>),
    mc_worker_api:delete(Connection, Collection, {}).

collection(Query, Config) ->
    case Query of
    superquery ->
        list_to_binary(get_value(collection, Config, "mqtt_user"));
    authquery ->
        list_to_binary(get_value(collection, Config, "mqtt_user"));
    aclquery ->
        list_to_binary(get_value(collection, Config, "mqtt_acl"))
    end.

ct_log(Connection, Collection, User1) ->
    Selector = {list_to_binary("username"), list_to_binary("%u")},
    Res = find(Connection, Collection, emq_auth_mongo:replvar(Selector, User1)),
    ct:log("Got:~p", [Res]).

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

