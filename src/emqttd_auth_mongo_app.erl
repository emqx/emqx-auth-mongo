%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
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

%% @doc MongoDB Plugin Application
%%
%% @author @lovecc0923
%% @author Feng Lee <feng@emqtt.io>
-module(emqttd_auth_mongo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, emqttd_plugin_mongo).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    application:ensure_all_started(mongodb),
    application:ensure_all_started(ecpool),
    {ok, Sup} = emqttd_plugin_mongo_sup:start_link(),
    register_auth_mod(),
    {ok, Sup}.

register_auth_mod() ->
    UserColl = list_to_binary(application:get_env(?APP, user_collection, "mqtt_user")),
    PassHash = application:get_env(?APP, password_hash, sha256),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_mongo, {UserColl, PassHash}).

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mongo).

