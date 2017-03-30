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

%% @doc Authentication with MongoDB.
-module(emq_auth_mongo).

-behaviour(emqttd_auth_mod).

-include("emq_auth_mongo.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-behaviour(ecpool_worker).

-export([replvar/2, connect/1, query/2]).

-record(state, {authquery, superquery}).
 
-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

%%--------------------------------------------------------------------
%% Auth Mod Callback
%%--------------------------------------------------------------------

init({AuthQuery, SuperQuery}) ->
  {ok, #state{authquery = AuthQuery, superquery = SuperQuery}}.

check(#mqtt_client{username = Username}, Password, _State) when ?EMPTY(Username); ?EMPTY(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, #state{authquery = AuthQuery, superquery = SuperQuery}) ->
    #authquery{collection = Collection, field = Field,
               hash = HashType, selector = Selector} = AuthQuery,
    case query(Collection, replvar(Selector, Client)) of
        undefined -> ignore;
        UserMap ->
            case maps:get(Field, UserMap) of
                #{} -> {error, password_error};
                PassHash  -> 
                    case check_pass(PassHash, Password, HashType) of
                        ok -> {ok, is_superuser(SuperQuery, Client)};
                        Error -> Error
                    end
             end
    end.

check_pass(PassHash, Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end.

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with MongoDB".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | list(), mqtt_client()) -> boolean()).
is_superuser(undefined, _MqttClient) ->
    false;
is_superuser(#superquery{collection = Coll, field = Field, selector = Selector}, Client) ->
    Row = query(Coll, replvar(Selector, Client)),
    case maps:get(Field, Row, false) of
        true   -> true;
        _False -> false
    end.

replvar({Field, <<"%u">>}, #mqtt_client{username = Username}) ->
    {Field, Username};
replvar({Field, <<"%c">>}, #mqtt_client{client_id = ClientId}) ->
    {Field, ClientId};
replvar(Selector, _Client) ->
    Selector.

%%--------------------------------------------------------------------
%% MongoDB Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    mc_worker_api:connect(fixopt(Opts, [])).

fixopt([], Acc) ->
    Acc;

fixopt([{login, Login} | Opts], Acc) when is_list(Login) ->
    fixopt(Opts, [{login, list_to_binary(Login)} | Acc]);
    
fixopt([{password, Passwd} | Opts], Acc) when is_list(Passwd) ->
    fixopt(Opts, [{password, list_to_binary(Passwd)} | Acc]);

fixopt([{database, DB} | Opts], Acc) when is_list(DB) ->
    fixopt(Opts, [{database, list_to_binary(DB)} | Acc]);

fixopt([Opt | Opts], Acc) ->
    fixopt(Opts, [Opt | Acc]).

query(Collection, Selector) ->
    ecpool:with_client(?APP, fun(Conn) -> mc_worker_api:find_one(Conn, Collection, Selector) end).

