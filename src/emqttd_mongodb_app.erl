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
%%% @doc MongoDB Plugin Application
%%% 
%%% @author @lovecc0923
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------

-module(emqttd_mongodb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, emqttd_mongodb).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(mongodb),
    application:ensure_all_started(ecpool),
    {ok, Sup} = emqttd_mongodb_sup:start_link(),
    register_auth_mod(),
    {ok, Sup}.

register_auth_mod() ->
    UserColl = list_to_binary(application:get_env(?APP, user_collection, "mqtt_user")),
    PassHash = application:get_env(?APP, password_hash, sha256),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_mongodb, {UserColl, PassHash}).

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mongodb).

