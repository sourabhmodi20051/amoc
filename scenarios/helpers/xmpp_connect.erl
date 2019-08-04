%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(xmpp_connect).

-export([connect_to_xmpp/2]).

-spec connect_to_xmpp(binary(),binary()) ->
    binary().
connect_to_xmpp(UserId, Password) -> 
    ExtraSpec = send_and_recv_escalus_handlers(),
    User = make_user(UserId, Password, ExtraSpec),
    {ok, Client, _} = amoc_xmpp:connect_or_exit(User),
    escalus_session:send_presence_available(Client),
    lager:info("~p connected", [UserId]),
    Client.

-spec user_spec(binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password) ->
    [ {username, ProfileId},
      {server, <<"localhost">>},
      {host, <<"127.0.0.1">>},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, base64:encode(crypto:strong_rand_bytes(5))}].

make_user(Id, Pass, Props) ->
    DefaultSpec = maps:from_list(user_spec(Id, Pass)),
    ExtraSpec = maps:from_list(Props),
    Merged = maps:merge(DefaultSpec, ExtraSpec),
    maps:to_list(Merged).

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [
      {received_stanza_handlers, [fun amoc_xmpp_handlers:measure_ttd/3]},
      {sent_stanza_handlers, [fun amoc_xmpp_handlers:measure_sent_messages/2]}
    ].
