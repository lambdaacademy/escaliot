-module(escaliot).

-export([connect/4, connect/5, close/1, subscribe_to_messages_from/3, cancel_subscription/2]).
%-export([subscribe_to_messages_from/3, cancel_subscription/2]).
-export([join_room/3]).
-export([send_message/3]).%%, sent_groupchat_message/3]).

-export_type([connection/0]).

-type connection() :: pid().
-type handler_function() :: fun( (binary()) -> ok ).
-type connection_options() :: [connection_option()].
-type connection_option() :: {atom(), any()}. %% escalus options


-spec connect(binary(), binary(), binary(), binary()) -> {ok, connection()} | {error, any()}.
connect(Username, Domain, Password, Resource) ->
    connect(Username, Domain, Password, Resource, []).

-spec connect(binary(), binary(), binary(), binary(), connection_options()) ->
    {ok, connection()} | {error, any()}.
connect(Username, Domain, Password, Resource,  Opts) ->
    escaliot_conn:start_link(Username, Domain, Password, Resource, Opts).

-spec join_room(connection(), binary(), binary()) -> ok | {error, any()}.
join_room(Connection, RoomJid, Nick) ->
    escaliot_conn:join_room(Connection, RoomJid, Nick).

-spec send_message(connection(), binary(), binary()) -> ok | {error, any()}.
send_message(Connection, To , Content) ->
    escaliot_conn:send_message(Connection, To, Content).

-spec subscribe_to_messages_from(connection(), binary(), handler_function()) -> {ok, reference()}.  %| {error, any()}.
subscribe_to_messages_from(Connection, JID, Handler) ->
    escaliot_conn:subscribe_to_messages_from(Connection, JID, Handler).

-spec cancel_subscription(connection(), reference()) -> ok | {error, subscription_doesnt_exist}.
cancel_subscription(Connection, Reference) ->
    escaliot_conn:cancel_subscription(Connection, Reference).

-spec close(connection()) -> ok.
close(Connection) ->
    escaliot_conn:stop(Connection).
