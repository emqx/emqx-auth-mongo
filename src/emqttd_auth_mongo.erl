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

%% @doc Authentication with MongoDB.
-module(emqttd_auth_mongo).

-author("Feng Lee<feng@emqtt.io").

-behaviour(emqttd_auth_mod).

-include("emqttd_plugin_mongo.hrl").

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {superquery, authquery}).
 
-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({SuperQuery, AuthQuery}) ->
  {ok, #state{superquery = SuperQuery, authquery = AuthQuery}}.

check(#mqtt_client{username = Username}, _Password, _State) when ?EMPTY(Username) ->
    {error, username_undefined};

check(Client, Password, #state{superquery = SuperQuery}) when ?EMPTY(Password) ->
    case emqttd_plugin_mongo:is_superuser(SuperQuery, Client) of
        true  -> ok;
        false -> {error, password_undefined}
    end;

check(Client, Password, #state{superquery = SuperQuery,authquery = AuthQuery}) ->
    #authquery{collection = Collection, field = Field,
               hash = HashType, selector = Selector} = AuthQuery,
    case emqttd_plugin_mongo:is_superuser(SuperQuery, Client) of
        false -> Selector1 = emqttd_plugin_mongo:replvar(Selector, Client),
                 UserMap   = emqttd_plugin_mongo:query(Collection, Selector1),
                 case maps:get(Field, UserMap, undefined) of
                     undefined -> {error, notfound};
                     PassHash  -> check_pass(PassHash, Password, HashType)
                 end;
        true  -> ok
    end.

check_pass(PassHash, Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end.

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with MongoDB".

