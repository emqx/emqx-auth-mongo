-module(emqttd_mongodb).

%% -include_lib("emqttd/include/emqttd.hrl").
-include_lib("../../../include/emqttd.hrl").

-export([onload/1, onunload/0]).

-export([on_message_publish/2]).

%% Called when the plugin application start
onload(Env) ->
  emqttd_broker:hook('message.publish', {?MODULE, on_message_publish},
    {?MODULE, on_message_publish, [Env]}).

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  Message;

on_message_publish(Message, _Env) ->
  io:format("publish ~s~n", [emqttd_message:format(Message)]),
  %%TODO: Store the message to mongodb
  Message.

%% Called when the plugin application stop
onunload() ->
  emqttd_broker:unhook('message.publish', {?MODULE, on_message_publish}).

