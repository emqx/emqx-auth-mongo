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

-module(emqttd_auth_mongo_app).

-author("Feng Lee<feng@emqtt.io").

-include("emqttd_auth_mongo.hrl").

-behaviour(application).

-import(proplists, [get_value/3]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    gen_conf:init(?APP),
    application:ensure_all_started(mongodb),
    {ok, Sup} = emqttd_auth_mongo_sup:start_link(),
    if_enabled(authquery, fun(AuthQuery) ->
        SuperQuery = r(superquery, gen_conf:value(?APP, superquery, undefined)),
        emqttd_access_control:register_mod(auth, emqttd_auth_mongo, {AuthQuery, SuperQuery})
    end),
    if_enabled(aclquery, fun(AclQuery) ->
        {ok, AclNomatch} = gen_conf:value(?APP, acl_nomatch),
        emqttd_access_control:register_mod(acl, emqttd_acl_mongo, {AclQuery, AclNomatch})
    end),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emqttd_acl_mongo),
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mongo),
    State.

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

if_enabled(Name, Fun) ->
    case gen_conf:value(?APP, Name) of
        {ok, Config} -> Fun(r(Name, Config));
        undefined    -> ok
    end.

r(_, undefined) -> undefined;

r(superquery, Config) ->
    #superquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                field      = list_to_binary(get_value(super_field, Config, "is_superuser")),
                selector   = binary_selector(get_value(selector, Config, {"username", "%u"}))};

r(authquery, Config) ->
    #authquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
               field      = list_to_binary(get_value(password_field, Config, "password")),
               hash       = get_value(password_hash, Config, sha256),
               selector   = binary_selector(get_value(selector, Config, {"username", "%u"}))};

r(aclquery, Config) ->
    #aclquery{collection = list_to_binary(get_value(collection, Config, "mqtt_acl")),
              selector   = binary_selector(get_value(selector, Config, {"username", "%u"}))}.

binary_selector({Field, Val}) ->
    {list_to_binary(Field), case is_list(Val) of true -> list_to_binary(Val); false -> Val end}.

