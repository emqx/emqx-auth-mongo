%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emqx_auth_mongo_app).

-behaviour(application).

-author("Feng Lee<feng@emqtt.io").

-include("emqx_auth_mongo.hrl").

-import(proplists, [get_value/3]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_mongo_sup:start_link(),
    with_env(auth_query, fun reg_authmod/1),
    with_env(acl_query,  fun reg_aclmod/1),
    emqx_auth_mongo_cfg:register(),
    {ok, Sup}.

prep_stop(State) ->
    emqx_access_control:unregister_mod(acl, emqx_acl_mongo),
    emqx_access_control:unregister_mod(auth, emqx_auth_mongo),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthQuery) ->
    SuperQuery = r(super_query, application:get_env(?APP, super_query, undefined)),
    emqx_access_control:register_mod(auth, emqx_auth_mongo, {AuthQuery, SuperQuery}),
    emqx_auth_mongo_cfg:unregister().

reg_aclmod(AclQuery) ->
    emqx_access_control:register_mod(acl, emqx_acl_mongo, AclQuery).

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
                selector   = parse_selector(get_value(selector, Config, {"username", "%u"}))};

r(auth_query, Config) ->
    #authquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
               field      = get_value(password_field, Config, [<<"password">>]),
               hash       = get_value(password_hash, Config, sha256),
               selector   = parse_selector(get_value(selector, Config, {"username", "%u"}))};

r(acl_query, Config) ->
    #aclquery{collection = list_to_binary(get_value(collection, Config, "mqtt_acl")),
              selector   = parse_selector(get_value(selector, Config, {"username", "%u"}))}.

parse_selector(Selector) ->
    case string:tokens(Selector, "=") of
        [Field, Val] -> {list_to_binary(Field), list_to_binary(Val)};
        _ -> {<<"username">>, <<"%u">>}
    end.

