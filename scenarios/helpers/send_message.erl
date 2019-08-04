-module(send_message).

-export([send_messages_many_times/5]).

-define(HOST, <<"localhost">>).

-spec send_messages_many_times(escalus:client(), timeout(), [binary()], binary(), number()) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds, Message, MessageCount) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval, Message)
        end,
    lists:foreach(S, lists:seq(1, MessageCount)).

-spec send_messages_to_neighbors(escalus:client(), [binary()], timeout(), binary()) -> list().
send_messages_to_neighbors(Client,TargetIds, SleepTime, Message) ->
    [send_message(Client, make_jid(TargetId), SleepTime, Message)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), binary(), timeout(), binary()) -> ok.
send_message(Client, ToId, SleepTime, Message) ->
    Msg = make_message(ToId, Message),
    escalus_connection:send(Client, Msg),
    lager:info("Message Sent"),
    timer:sleep(SleepTime).

-spec make_message(binary(), binary()) -> exml:element().
make_message(ToId, Message) ->
    %Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Body = Message,
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Body), Id).

-spec make_jid(binary()) -> binary().
make_jid(Id) ->
    %BinInt = integer_to_binary(Id),
    %ProfileId = <<"user_", BinInt/binary>>,
    ProfileId = Id,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.
