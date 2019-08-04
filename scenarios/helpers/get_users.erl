%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(get_users).

-export([get_users/0]).

-spec get_users() -> 
    any().
get_users() ->
    Lines = read_file:readlines('users.txt'),
    LinesList = string:tokens(Lines,"\n"),
    Users = getUsers(LinesList).

getUsers([H|T]) ->
    getUser(H) ++ getUsers(T);
getUsers([]) ->
     [].

getUser(H) ->
    [Username,Password] = string:tokens(H,","),
    BinaryUsername = list_to_binary(Username),
    BinaryPassword = list_to_binary(Password),
    [[BinaryUsername, BinaryPassword]].
