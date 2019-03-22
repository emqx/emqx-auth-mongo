%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_mongo_cfg).

-export([ register/0
        , unregister/0
        ]).

-include("emqx_auth_mongo.hrl").

register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_formatter(),
    register_config().

unregister() ->
    unregister_formatter(),
    unregister_config(),
    clique_config:unload_schema(?APP).

%%--------------------------------------------------------------------

register_formatter() ->
    [clique:register_formatter(cuttlefish_variable:tokenize(Key),
     fun formatter_callback/2) || Key <- keys()].

formatter_callback([_, _, "server"], Params) ->
    format(proplists:get_value(hosts, Params));
formatter_callback([_, _, "pool"], Params) ->
    proplists:get_value(pool_size, Params);
formatter_callback([_, _, "database"], Params) ->
    proplists:get_value(database, proplists:get_value(worker_options, Params));
formatter_callback([_, _, "auth_source"], Params) ->
    proplists:get_value(auth_source, proplists:get_value(worker_options, Params));
formatter_callback([_, _, "login"], Params) ->
    proplists:get_value(login, proplists:get_value(worker_options, Params));
formatter_callback([_, _, "password"], Params) ->
    proplists:get_value(password, proplists:get_value(worker_options, Params));
formatter_callback([_, _, Key], Params) ->
    proplists:get_value(list_to_atom(Key), Params);
formatter_callback([_, _, _, "password_hash"], Params) ->
    format(tuple_or_atom_to_list(proplists:get_value(password_hash, Params)));
formatter_callback([_, _, _, "password_field"], Params) ->
    format([binary_to_list(Field) || Field <- proplists:get_value(password_field, Params)]);
formatter_callback([_, _, _, Key], Params) ->
    proplists:get_value(list_to_atom(Key), Params).

tuple_or_atom_to_list(Value) when is_tuple(Value) ->
    tuple_to_list(Value);
tuple_or_atom_to_list(Value) when is_atom(Value) ->
    atom_to_list(Value).

unregister_formatter() ->
    [clique:unregister_formatter(cuttlefish_variable:tokenize(Key)) || Key <- keys()].

register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, "server"], Value0) ->
    {ok, Env} = application:get_env(?APP, server),
    Value = string:tokens(Value0, ","),
    application:set_env(?APP, server, lists:keyreplace(hosts, 1, Env, {hosts, Value})),
    " successfully\n";

config_callback([_, _, "pool"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(pool_size, 1, Env, {pool_size, Value})),
    " successfully\n";
config_callback([_, _, "database"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    Env1 = proplists:get_value(worker_options, Env),
    Env2 = lists:keyreplace(database, 1, Env1, {database, list_to_binary(Value)}),
    application:set_env(?APP, server, lists:keyreplace(worker_options, 1, Env, {worker_options, Env2})),
    " successfully\n";
config_callback([_, _, "auth_source"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    Env1 = proplists:get_value(worker_options, Env),
    Env2 = lists:keyreplace(auth_source, 1, Env1, {auth_source, list_to_binary(Value)}),
    application:set_env(?APP, server, lists:keyreplace(worker_options, 1, Env, {worker_options, Env2})),
    " successfully\n";
config_callback([_, _, "login"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    Env1 = proplists:get_value(worker_options, Env),
    Env2 = lists:keyreplace(login, 1, Env1, {login, list_to_binary(Value)}),
    application:set_env(?APP, server, lists:keyreplace(worker_options, 1, Env, {worker_options, Env2})),
    " successfully\n";
config_callback([_, _, "password"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    Env1 = proplists:get_value(worker_options, Env),
    Env2 = lists:keyreplace(password, 1, Env1, {password, list_to_binary(Value)}),
    application:set_env(?APP, server, lists:keyreplace(worker_options, 1, Env, {worker_options, Env2})),
    " successfully\n";
config_callback([_, _, Key0], Value) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(Key, 1, Env, {Key, Value})),
    " successfully\n";

config_callback([_, _, Key0, "password_hash"], Value0) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, Key),
    Value = parse_password_hash(Value0),
    application:set_env(?APP, Key, lists:keyreplace(password_hash, 1, Env, {password_hash, Value})),
    " successfully\n";

config_callback([_, _, Key0, "password_field"], Value0) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, Key),
    Value = parse_password_field(Value0),
    application:set_env(?APP, Key, lists:keyreplace(password_field, 1, Env, {password_field, Value})),
    " successfully\n";

config_callback([_, _, Key0, Key1], Value) ->
    Key2 = list_to_atom(Key0),
    Key3 = list_to_atom(Key1),
    {ok, Env} = application:get_env(?APP, Key2),
    application:set_env(?APP, Key2, lists:keyreplace(Key3, 1, Env, {Key3, Value})),
    " successfully\n".

%%--------------------------------------------------------------------
%% UnRegister config
%%--------------------------------------------------------------------
unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

%%--------------------------------------------------------------------

keys() ->
    ["auth.mongo.type",
     "auth.mongo.server",
     "auth.mongo.pool",
     "auth.mongo.login",
     "auth.mongo.password",
     "auth.mongo.database",
     "auth.mongo.auth_source",
     "auth.mongo.auth_query.collection",
     "auth.mongo.auth_query.password_field",
     "auth.mongo.auth_query.password_hash",
     "auth.mongo.auth_query.selector",
     "auth.mongo.super_query.collection",
     "auth.mongo.super_query.super_field",
     "auth.mongo.super_query.selector",
     "auth.mongo.acl_query.collection",
     "auth.mongo.acl_query.selector"].

format(Value) ->
    format(Value, "").
format([Head], Acc) ->
    lists:concat([Acc, Head]);
format([Head | Tail], Acc) ->
    format(Tail, Acc ++ lists:concat([Head, ","])).

parse_password_hash(Value) ->
    case string:tokens(Value, ",") of
          [Hash]           -> list_to_atom(Hash);
          [Prefix, Suffix] -> {list_to_atom(Prefix), list_to_atom(Suffix)};
          [Hash, MacFun, Iterations, Dklen] -> {list_to_atom(Hash),
                                                list_to_atom(MacFun),
                                                list_to_integer(Iterations),
                                                list_to_integer(Dklen)};
          _                -> plain
    end.

parse_password_field(Value) ->
    [list_to_binary(Field) || Field <- string:tokens(Value, ",")].
