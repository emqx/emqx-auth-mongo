%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015-2016 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc MongoDB Pool Client
%%% 
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------

-module(emqttd_mongodb_client).

-behaviour(ecpool_worker).

-export([connect/1, query/2]).

-define(POOL, mongodb_pool).

connect(Opts) ->
    mongo:connect(case lists:keyfind(database, 1, Opts) of
            {database, DB} -> [{database, list_to_binary(DB)} | lists:keydelete(database, 1, Opts)];
            fasle          -> Opts
        end).

query(Collection, Where) ->
    ecpool:with_client(?POOL, fun(Conn) ->
          Cursor = mongo:find(Conn, Collection, Where),
          Result = mc_cursor:rest(Cursor),
          mc_cursor:close(Cursor),
          {ok, Result}
        end).

