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

%% @doc MongoDB Pool Client
%% @author Feng Lee <feng@emqtt.io>
-module(emqttd_mongo_client).

-behaviour(ecpool_worker).

-export([connect/1, query/2]).

-define(POOL, mongo_pool).

connect(Opts) ->
    mongo:connect(case lists:keyfind(database, 1, Opts) of
            {database, DB} -> [{database, iolist_to_binary(DB)} | lists:keydelete(database, 1, Opts)];
            fasle          -> Opts
        end).

query(Collection, Where) ->
    ecpool:with_client(?POOL, fun(Conn) ->
          Cursor = mongo:find(Conn, Collection, Where),
          Result = mc_cursor:rest(Cursor),
          mc_cursor:close(Cursor),
          {ok, Result}
        end).

