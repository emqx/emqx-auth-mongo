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
  store(Message),
  Message.

%% Called when the plugin application stop
onunload() ->
  emqttd_broker:unhook('message.publish', {?MODULE, on_message_publish}).

%%    msgid           :: mqtt_msgid(),      %% Global unique message ID
%%    pktid           :: mqtt_pktid(),      %% PacketId
%%    topic           :: binary(),          %% Topic that the message is published to
%%    from            :: binary() | atom(), %% ClientId of publisher
%%    qos    = 0      :: 0 | 1 | 2,         %% Message QoS
%%    retain = false  :: boolean(),         %% Retain flag
%%    dup    = false  :: boolean(),         %% Dup flag
%%    sys    = false  :: boolean(),         %% $SYS flag
%%    payload         :: binary(),          %% Payload
%%    timestamp       :: erlang:timestamp() %% os:timestamp
store(#mqtt_message{msgid = MsgId, pktid = PktId, payload = Payload, timestamp = Timestamp}) ->
  [{_, Body}, {_, Direction}, {_, From}, {_, Nickname}, {_, SubType}, {_, Time}, {_, To}, {_, Topic}, {_, Type}] = jsx:decode(Payload),
  Msg1 = #{
    <<"body">> => Body,
    <<"direction">> => Direction,
    <<"from">> => From,
    <<"nickname">> => Nickname,
    <<"subType">> => SubType,
    <<"time">> => Time,
    <<"to">> => To,
    <<"topic">> => Topic,
    <<"type">> => Type
  },
  Msg2 = #{
    <<"body">> => Body,
    <<"direction">> => 1,
    <<"from">> => To,
    <<"nickname">> => Nickname,
    <<"subType">> => SubType,
    <<"time">> => Time,
    <<"to">> => From,
    <<"topic">> => Topic,
    <<"type">> => Type
  },
  {ok, Connection} = mongo:connect([{database, <<"db0">>}]),
  mongo:insert(Connection, <<"mqtt_message">>, [Msg1, Msg2]).