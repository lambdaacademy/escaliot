-module(escaliot_conn).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

%% API
-export([start_link/5]).

-export([send_message/3]).
-export([join_room/3, register_handler/3, unregister_handler/2, filter_by_jid/2, subscribe_to_messages_from/3, cancel_subscription/2]).
-export([stop/1]).
-define(RETURN, ok). % Return value for subscripted handler when sender does not match.

-record(state, {client,handlers}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Username, Domain, Password, Resource, Opts) ->
    F = fun () ->
                init([Username, Domain, Password, Resource, Opts])
        end,
    Pid =  spawn_link(F),
    {ok, Pid}.
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [Username, Domain, Password, Resource, Opts], []).

stop(Connection) ->
    Connection ! stop,
    ok.

send_message(Connection, To, Content) ->
    Msg = {send_message, Content, To},
    simple_call(Connection, Msg).

join_room(Connection, RoomJid, Nick) ->
    Msg = {join_room, RoomJid, Nick},
    simple_call(Connection, Msg).

register_handler(Connection, HandlerName, Handler) ->
    Msg = {register_handler, HandlerName, Handler},
    simple_call(Connection, Msg).

unregister_handler(Connection, Handler) ->
  Msg = {unregister_handler, Handler},
  simple_call(Connection, Msg).


subscribe_to_messages_from(Connection, all, Handler) ->         % It is normal handler but checks sender before handling message.
  Fun = fun(Msg) ->
    case escalus_pred:is_message(Msg) of
      true ->
        Handler(Msg);
      _ -> ?RETURN
    end
  end,
  Ref = make_ref(),
  register_handler(Connection, Ref, Fun),
  {ok, Ref};

subscribe_to_messages_from(Connection, JID, Handler) ->
  Fun = fun(Msg) ->
    case filter_by_jid(Msg, JID) of
      true ->
      Handler(Msg);
      _ -> ok
    end
  end,
  Ref = make_ref(),
  register_handler(Connection, Ref, Fun),
  {ok, Ref}.

cancel_subscription(Connection, JID) ->
  case unregister_handler(Connection, JID) of
    ok -> ok;
    _ -> {error, subscription_doesnt_exist}
  end.

%%%===================================================================
%%%
%%%===================================================================

init([Username, Domain, Password, Resource, Opts]) ->
    Cfg = user_spec(Username, Domain, Password, Resource),
    MergedConf = merge_props(Opts, Cfg),
    case escalus_connection:start(MergedConf) of
        {ok, Client, _, _} ->
            send_presence_available(Client),
            loop(Client, dict:new(), []);
        _ ->
            exit(cannot_connect)
    end.

loop(Client, Handlers, JoinedRooms) ->
    receive
        {{send_message, Text, To}, FromPid} ->
            M = case proplists:get_value(To, JoinedRooms, undefined) of
                    undefined ->
                        escalus_stanza:chat_to(To, Text);
                    Nick ->
                        escalus_stanza:groupchat_to(To, Text)
                end,
            escalus:send(Client, M),
            FromPid ! ok,
            loop(Client, Handlers, JoinedRooms);
        {{join_room, Room, Nick}, FromPid} ->
            escalus:send(Client, stanza_muc_enter_room(Room, Nick)),
            NewRooms = [{Room, Nick} | JoinedRooms],
            FromPid ! ok,
            loop(Client, Handlers, NewRooms);
        {{register_handler, HandlerName, Handler}, FromPid} ->
            NewHandlers = dict:append(HandlerName, Handler, Handlers),
            FromPid ! ok,
            loop(Client, NewHandlers, JoinedRooms);
      {{unregister_handler, HandlerName}, FromPid} ->
        case dict:find(HandlerName, Handlers) of
          error ->
          FromPid ! {error, no_registered_handler},
          loop(Client, Handlers, JoinedRooms);
          _ ->
        NewHandlers = dict:erase(HandlerName, Handlers),
        FromPid ! ok,
        loop(Client, NewHandlers, JoinedRooms)
        end;
        stop ->
            escalus_client:stop(Client),
            ok;
      {stanza, _, Stanza} ->
            P = handle_stanza(Stanza, Handlers),
            F = io_lib:format("~p",[P]),
            io:format("~p",[lists:flatten(F)]),
            loop(Client, Handlers, JoinedRooms);
      _ ->
              loop(Client, Handlers, JoinedRooms)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

simple_call(Connection, Msg) ->
    Connection ! {Msg, self()},
    receive
        R ->  R
    after 5000 ->
              {error, timeout}
    end.

filter_by_jid(Stanza, Jid) ->
  escalus_pred:is_message(Stanza) and escalus_pred:is_stanza_from(Jid, Stanza).

handle_stanza(Stanza, Handlers) ->
  dict:fold(fun(_, Handler, Acc) -> [(hd(Handler))(Stanza)] ++ Acc end, [], Handlers).

user_spec(Username, Domain, Password, Resource) ->
    [ {username, Username},
      {server, Domain},
      {host, Domain},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Resource}
    ].


send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(<<"available">>,
                                [#xmlel{ name = <<"x">>,
                                         attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

room_address(Room, Nick) ->
    <<Room/binary, "/", Nick/binary>>.

merge_props(New, Old) ->
    lists:foldl(fun({K, _}=El, Acc) ->
                        lists:keystore(K, 1, Acc, El)
                end, Old, New).


