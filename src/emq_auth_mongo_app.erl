%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emq_auth_mongo_app).

-author("Feng Lee<feng@emqtt.io").

-include("emq_auth_mongo.hrl").

-behaviour(application).

-import(proplists, [get_value/3]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_mongo_sup:start_link(),
    with_env(auth_query, fun reg_authmod/1),
    with_env(acl_query,  fun reg_aclmod/1),
    emq_auth_mongo_config:register(),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emq_acl_mongo),
    emqttd_access_control:unregister_mod(auth, emq_auth_mongo),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthQuery) ->
    SuperQuery = r(super_query, application:get_env(?APP, super_query, undefined)),
    emqttd_access_control:register_mod(auth, emq_auth_mongo, {AuthQuery, SuperQuery}),
    emq_auth_mongo_config:unregister().

reg_aclmod(AclQuery) ->
    emqttd_access_control:register_mod(acl, emq_acl_mongo, AclQuery).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

with_env(Name, Fun) ->
    case application:get_env(?APP, Name) of
        undefined    -> ok;
        {ok, Config} -> Fun(r(Name, Config))
    end.

r(super_query, undefined) -> 
    undefined;
r(super_query, Config) ->
    #superquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                field      = list_to_binary(get_value(super_field, Config, "is_superuser")),
                selector   = get_value(selector, Config, ?DEFAULT_SELECTORS)};

r(auth_query, Config) ->
    #authquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
               field      = get_value(password_field, Config, [<<"password">>]),
               hash       = get_value(password_hash, Config, sha256),
               selector   = get_value(selector, Config, ?DEFAULT_SELECTORS)};

r(acl_query, Config) ->
    #aclquery{collection = list_to_binary(get_value(collection, Config, "mqtt_acl")),
              selector   = get_value(selector, Config, ?DEFAULT_SELECTORS)}.
