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

%% @doc MongoDB Plugin
-module(emqttd_plugin_mongo).

-author("Feng Lee<feng@emqtt.io>").

-behaviour(ecpool_worker).

-include("emqttd_plugin_mongo.hrl").

-include("../../../include/emqttd.hrl").

-import(proplists, [get_value/3]).

-export([config/1, is_superuser/2, replvar/2, connect/1, query/2]).

-define(APP, ?MODULE).

%%--------------------------------------------------------------------
%% Config
%%--------------------------------------------------------------------

config(superquery) ->
    with_env(superquery, fun(Config) ->
        #superquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                    field      = list_to_binary(get_value(super_field, Config, "is_superuser")),
                    selector   = binary_selector(get_value(selector, Config, {"username", "%u"}))}
    end);

config(authquery) ->
    with_env(authquery, fun(Config) ->
        #authquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                   field      = list_to_binary(get_value(password_field, Config, "password")),
                   hash       = get_value(password_hash, Config, sha256),
                   selector   = binary_selector(get_value(selector, Config, {"username", "%u"}))}
    end);

config(aclquery) ->
    with_env(aclquery, fun(Config) ->
        #aclquery{collection = list_to_binary(get_value(collection, Config, "mqtt_user")),
                   selector  = binary_selector(get_value(selector, Config, {"username", "%u"}))}
    end);

config(Key) ->
    with_env(Key, fun(Env) -> Env end).

with_env(Key, Fun) ->
    case application:get_env(?APP, Key) of
        {ok, Env}   -> Fun(Env);
        undefined -> undefined
    end.

binary_selector({Field, Val}) ->
    {list_to_binary(Field), case is_list(Val) of true -> list_to_binary(Val); false -> Val end}.

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
    ecpool:with_client(?MODULE, fun(Conn) -> mc_worker_api:find_one(Conn, Collection, Selector) end).

