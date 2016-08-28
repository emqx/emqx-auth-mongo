%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_mongo_SUITE).

-compile(export_all).

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("common_test/include/ct.hrl").

-include("emqttd_auth_mongo.hrl").

-define(INIT_ACL, [{<<"username">>, <<"testuser">>, <<"clientid">>, <<"null">>, <<"subscribe">>, [<<"#">>]},
                   {<<"username">>, <<"dashboard">>, <<"clientid">>, <<"null">>, <<"pubsub">>, [<<"$SYS/#">>]}]).

all() -> 
    [{group, emqttd_auth_mongo}].

groups() -> 
    [{emqttd_auth_mongo, [sequence],
     [check_acl]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_auth_mongo, conf, filename:join([DataDir, "emqttd_auth_mongo.conf"])),
    application:ensure_all_started(emqttd_auth_mongo),
    {ok, Connection} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, emqttd_auth_mongo})),
    [{connection, Connection} | Config].

end_per_suite(_Config) ->
    application:stop(emqttd_auth_mongo),
    application:stop(ecpool),
    application:stop(mongodb),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(Config) ->
    Connection = proplists:get_value(connection, Config),
    Collection = collection(aclquery),
    mc_worker_api:delete(Connection, Collection, {}),
    mc_worker_api:insert(Connection, Collection, ?INIT_ACL),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"dashboard">>},
    2 = mc_worker_api:count(Connection, Collection, {}),
    %% ct log output
    %%ct_log(Connection, Collection, User1),
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    deny = emqttd_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
    allow = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>),
    mc_worker_api:delete(Connection, Collection, {}).

check_auth(_) ->
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser1">>},
    ok = emqttd_access_control:auth(User1, <<"pass1">>),
    {error, _} = emqttd_access_control:auth(User1, <<"pass">>).

collection(Query) ->
    case emqttd_auth_mongo:config(Query) of
    #authquery{collection = AuthConnection} ->
            AuthConnection;
    #aclquery{collection = AclConnection} ->
            AclConnection;
    #superquery{collection = SuperConnection} ->
            SuperConnection
    end.

ct_log(Connection, Collection, User1) ->
    Selector = {list_to_binary("username"), list_to_binary("%u")},
    Res = find(Connection, Collection, emqttd_auth_mongo:replvar(Selector, User1)),
    ct:log("Got:~p", [Res]).

%% @private
find(Connection, Collection, Selector) ->
    find(Connection, Collection, Selector, #{}).

find(Connection, Collection, Selector, Projector) ->
    Cursor = mc_worker_api:find(Connection, Collection, Selector, #{projector => Projector}),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.

