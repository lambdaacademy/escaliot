-module(escaliot_conn).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

%% API
-export([start_link/5]).

-export([send_message/3]).
-export([join_room/3]).
-export([stop/1]).

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
    Stanzas = escalus_client:wait_for_stanzas(Client, 1, 50),
%    maybe_handle_stanza(Stanzas, Handlers),
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
        {{register_handler, Handler}, _FromPid} ->
            NewHandlers = Handler,
            loop(Client, NewHandlers, JoinedRooms);
        stop ->
            escalus_client:stop(Client),
            ok
    after 0 ->
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


