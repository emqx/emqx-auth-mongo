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

-module(emqttd_plugin_mongo_app).

-author("Feng Lee<feng@emqtt.io").

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-define(APP, emqttd_plugin_mongo).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    application:ensure_all_started(mongodb),
    {ok, Sup} = emqttd_plugin_mongo_sup:start_link(),
    register_auth_mod(),
    {ok, Sup}.

register_auth_mod() ->
    SuperQuery = ?APP:config(superquery), AuthQuery = ?APP:config(authquery),
    AclQuery = ?APP:config(aclquery), AclNomatch = ?APP:config(acl_nomatch),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_mongo, {SuperQuery, AuthQuery}),
    if
        AclQuery == undefined ->
            ok;
        true ->
            AclEnv = {SuperQuery, AclQuery, AclNomatch},
            emqttd_access_control:register_mod(acl, emqttd_acl_mongo, AclEnv)
    end.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emqttd_acl_mongo),
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mongo),
    State.

stop(_State) ->
    ok.

