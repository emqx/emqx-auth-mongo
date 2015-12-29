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
  
store(#mqtt_message{msgid = MsgId, pktid = PktId, topic = Topic, from = From, payload = Payload}) ->
  [{_, Body1}, {_, Direction1}, {_, From1}, {_, Nickname1}, {_, SubType1}, {_, Time1}, {_, To1}, {_, Topic1}, {_, Type1}] = jsx:decode(Payload),
  %% 聊天记录存储
  Msg1 = #{
    <<"body">> => Body1,
    <<"direction">> => Direction1,
    <<"from">> => From1,
    <<"nickname">> => Nickname1,
    <<"subType">> => SubType1,
    <<"time">> => Time1,
    <<"to">> => To1,
    <<"topic">> => Topic1,
    <<"type">> => Type1
  },
  Msg2 = #{
    <<"body">> => Body1,
    <<"direction">> => 1,
    <<"from">> => To1,
    <<"nickname">> => Nickname1,
    <<"subType">> => SubType1,
    <<"time">> => Time1,
    <<"to">> => From1,
    <<"topic">> => Topic1,
    <<"type">> => Type1
  },
  Database = <<"db0">>,
  {ok, DBConnection} = mongo:connect([{database, Database}]),
  mongo:insert(DBConnection, <<"Message">>, [Msg1, Msg2]),
  %% 聊天记录存储
  
  %% 聊天列表记录存储
  Collection = <<"TopicHistory">>,
  UserId = From1,
  Id = Topic1,
  Map1 = #{
    <<"type">> 		=> type(binary_to_list(Topic1)),	%% 消息类型
    <<"userId">> 	=> UserId,		%% 消息发送者
    <<"id">> 		=> Topic1,		%% 主题
    <<"name">> 		=> Nickname1,	%% 聊天记录名字
    <<"desc">> 		=> Body1,		%% 聊天记录最后一条消息
    <<"time">> 		=> timestamp(),	%% 聊天记录最后一条消息发送时间
    <<"count">> 	=> 0			%% 未读消息数
  },
  case mongo:count(DBConnection, Collection, {<<"_id">>, UserId}) of
    0 ->
      mongo:insert(DBConnection, Collection, #{<<"_id">> => UserId, <<"topics">> => [Map1]});
    _ ->
	  case mongo:count(DBConnection, Collection, {<<"_id">>, UserId, <<"topics.id">>, Id}) of
		0 -> 
		Q = #{<<"_id">> => UserId},
	  	Ops = #{<<"$addToSet">> => #{<<"topics">> =>  #{<<"$each">> => [Map1]}}},
	  	mongo:update(DBConnection, Collection, Q, Ops);
		_ -> "exists,skip"
      end
  end,
  %% 聊天列表记录存储
  
  mongo:disconnect(DBConnection).
  
%% 获取用户Id
id(Topic) ->
	Index = string:chr(Topic, 47),
	string:substr(Topic, Index + 1).

%% 获取类型
type(Topic) ->
	Index = string:chr(Topic, 47),
	case string:substr(Topic, 1, Index - 1) of
		"USER" -> 1;
		"GROUP" -> 2;
		"SYSTEM" -> 3;
		_ -> 0
	end.

%% 获取当前时间秒数
timestamp() -> 
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.  
  
  