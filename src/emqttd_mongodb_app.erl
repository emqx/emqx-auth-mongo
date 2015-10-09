-module(emqttd_mongodb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Env = application:get_all_env(),
    emqttd_mongodb:onload(Env),
    emqttd_mongodb_sup:start_link().

stop(_State) ->
    emqttd_mongodb:onunload(), ok.
