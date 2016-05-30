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

%% @doc ACL with MongoDB.
-module(emqttd_acl_mongo).

-behaviour(emqttd_acl_mod).

-include("emqttd_plugin_mongo.hrl").

-include("../../../include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {superquery, aclquery, nomatch}).

init({SuperQuery, AclQuery, AclNomatch}) ->
    {ok, #state{superquery = SuperQuery, aclquery = AclQuery, nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{superquery = SuperQuery,
                                          aclquery   = AclQuery,
                                          nomatch    = Default}) ->
    case emqttd_plugin_mongo:is_superuser(SuperQuery, Client) of
        false -> #aclquery{collection = Coll, selector = Selector} = AclQuery,
                 Row = emqttd_plugin_mongo:query(Coll, emqttd_plugin_mongo:replvar(Selector, Client)),
                 case match(Client, Topic, topics(PubSub, Row)) of
                     matched -> allow;
                     nomatch -> Default
                 end;
        true  -> allow
    end.

match(_Client, _Topic, []) ->
    nomatch;
match(Client, Topic, [TopicFilter|More]) ->
    case emqttd_topic:match(Topic, feedvar(Client, TopicFilter)) of
        true  -> matched;
        false -> match(Client, Topic, More)
    end.

topics(publish, Row) ->
    lists:umerge(maps:get(<<"publish">>, Row, []), maps:get(<<"pubsub">>, Row, []));

topics(subscribe, Row) ->
    lists:umerge(maps:get(<<"subscribe">>, Row, []), maps:get(<<"pubsub">>, Row, [])).

feedvar(#mqtt_client{client_id = ClientId, username = Username}, Str) ->
    lists:foldl(fun({Var, Val}, Acc) ->
                    feedvar(Acc, Var, Val)
                end, Str, [{"%u", Username}, {"%c", ClientId}]).

feedvar(Str, _Var, undefined) ->
    Str;
feedvar(Str, Var, Val) ->
    re:replace(Str, Var, Val, [global, {return, binary}]).

reload_acl(_State) ->
    ok.

description() ->
    "ACL with MongoDB".

