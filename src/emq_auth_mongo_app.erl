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

-module(emq_auth_mongo_app).

-author("Feng Lee<feng@emqtt.io").

-include("emq_auth_mongo.hrl").

-behaviour(application).

-import(proplists, [get_value/3]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_mongo_sup:start_link(),
    if_enabled(auth_query, fun reg_authmod/1),
    if_enabled(acl_query,  fun reg_aclmod/1),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emq_acl_mongo),
    emqttd_access_control:unregister_mod(auth, emq_auth_mongo),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthQuery) ->
    SuperQuery = r(super_query, application:get_env(?APP, super_query, undefined)),
    emqttd_access_control:register_mod(auth, emq_auth_mongo, {AuthQuery, SuperQuery}).

reg_aclmod(AclQuery) ->
    emqttd_access_control:register_mod(acl, emq_acl_mongo, AclQuery).


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

if_enabled(Name, Fun) ->
    case application:get_env(?APP, Name) of
        {ok, Config} -> Fun(r(Name, Config));
        undefined    -> ok
    end.

r(_, undefined) -> undefined;

r(super_query, Config) ->
    #superquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                field      = list_to_binary(get_value(super_field, Config, "is_superuser")),
                selector   = parse_selector(get_value(selector, Config, {"username", "%u"}))};

r(auth_query, Config) ->
    #authquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
               field      = list_to_binary(get_value(password_field, Config, "password")),
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


