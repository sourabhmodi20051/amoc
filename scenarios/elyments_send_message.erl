-module(elyments_send_message).

-behavior(amoc_scenario).

-export([start/1]).
-export([init/0]).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 73).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 1000).

-spec init() -> ok.
init() ->
    http_req:start(),
    amoc_metrics:init(counters, amoc_metrics:messages_spiral_name()),
    amoc_metrics:init(times, amoc_metrics:message_ttd_histogram_name()),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    do_start(xmpp, MyId).

do_start(xmpp, MyId) ->
     Users = get_users:get_users(),
     IthUser = lists:nth(MyId, Users),
     [Username, Password] = IthUser,
     Client = xmpp_connect:connect_to_xmpp(Username, Password),
     NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-4),MyId+4)),
     NeighbourUserIds = lists:map(fun(Id) -> lists:nth(1,lists:nth(Id, Users)) end, NeighbourIds),
     Message = lists:nth(1,string:tokens(read_file:readlines('message.txt'),"\n")),
     send_message:send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourUserIds, Message, ?NUMBER_OF_SEND_MESSAGE_REPEATS).

