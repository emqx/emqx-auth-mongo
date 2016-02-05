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
%% @author @lovecc0923
%% @author Feng Lee <feng@emqtt.io>
-module(emqttd_auth_mongo).

-behaviour(emqttd_auth_mod).

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {collection, hash_type}).
 
-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({Collection, HashType}) ->
  {ok, #state{collection = Collection, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
  when ?EMPTY(Username) orelse ?EMPTY(Password) ->
  {error, undefined};

check(#mqtt_client{username = Username}, Password,
    #state{collection = Collection, hash_type = HashType}) ->
    case emqttd_mongo_client:query(Collection, {<<"username">>, Username}) of
        {ok, [Record]} ->
          check_pass(maps:find(<<"password">>, Record), Password, HashType);
        {ok, []} ->
          {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

check_pass({ok, PassHash}, Password, HashType) ->
  case PassHash =:= hash(HashType, Password) of
    true -> ok;
    false -> {error, password_error}
  end;

check_pass(error, _Password, _HashType) ->
    {error, not_found}.

description() -> "Authentication with MongoDB".

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

