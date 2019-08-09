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

-module(emqx_auth_mongo).

-include("emqx_auth_mongo.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ check/2
        , description/0]).

-behaviour(ecpool_worker).

-export([ replvar/2
        , replvars/2
        , connect/1
        , query/2
        , query_multi/2
        ]).

check(Credentials = #{password := Password}, #{authquery := AuthQuery, superquery := SuperQuery}) ->
    #authquery{collection = Collection, field = Fields,
               hash = HashType, selector = Selector} = AuthQuery,
    case query(Collection, maps:from_list(replvars(Selector, Credentials))) of
        undefined -> ok;
        UserMap ->
            Result = case [maps:get(Field, UserMap, undefined) || Field <- Fields] of
                        [undefined] -> {error, password_error};
                        [PassHash] ->
                            check_pass({PassHash, Password}, HashType);
                        [PassHash, Salt|_] ->
                            check_pass({PassHash, Salt, Password}, HashType)
                     end,
            case Result of
                ok -> {stop, Credentials#{is_superuser => is_superuser(SuperQuery, Credentials),
                                          anonymous => false,
                                          auth_result => success}};
                {error, Error} ->
                    ?LOG(error, "[MongoDB] check auth fail: ~p", [Error]),
                    {stop, Credentials#{auth_result => Error, anonymous => false}}
            end
    end.

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.

description() -> "Authentication with MongoDB".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | #superquery{}, emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) ->
    false;
is_superuser(#superquery{collection = Coll, field = Field, selector = Selector}, Credentials) ->
    Row = query(Coll, maps:from_list(replvars(Selector, Credentials))),
    case maps:get(Field, Row, false) of
        true   -> true;
        _False -> false
    end.

replvars(VarList, Credentials) ->
    lists:map(fun(Var) -> replvar(Var, Credentials) end, VarList).

replvar({Field, <<"%u">>}, #{username := Username}) ->
    {Field, Username};
replvar({Field, <<"%c">>}, #{client_id := ClientId}) ->
    {Field, ClientId};
replvar({Field, <<"%C">>}, #{cn := CN}) ->
    {Field, CN};
replvar({Field, <<"%d">>}, #{dn := DN}) ->
    {Field, DN};
replvar(Selector, _Client) ->
    Selector.

%%--------------------------------------------------------------------
%% MongoDB Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    Type = proplists:get_value(type, Opts, single),
    Hosts = proplists:get_value(hosts, Opts, []),
    Options = proplists:get_value(options, Opts, []),
    WorkerOptions = proplists:get_value(worker_options, Opts, []),
    mongo_api:connect(Type, Hosts, Options, WorkerOptions).

query(Collection, Selector) ->
    ecpool:with_client(?APP, fun(Conn) -> mongo_api:find_one(Conn, Collection, Selector, #{}) end).

query_multi(Collection, SelectorList) ->
    lists:foldr(fun(Selector, Acc) ->
        case query(Collection, Selector) of
            undefined -> Acc;
            Result -> [Result|Acc]
        end
    end, [], SelectorList).
