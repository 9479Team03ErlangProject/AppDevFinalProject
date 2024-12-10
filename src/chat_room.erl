%%%-------------------------------------------------------------------
%%% @author Arvin Malaluan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module implements a simple chat room functionality. It allows users
%%%  to join or leave a room, send messages, and handle crashes with recovery.
%%% @end
%%% Created : 01. Dec 2024 5:27 pm
%%%-------------------------------------------------------------------

-module(chat_room).
-author("Arvin Malaluan").
-export([start/1, join/2, leave/2, send_message/2, crash_recovery/1]).

%% Helper function to ensure consistent RoomName conversion
to_atom(RoomName) when is_list(RoomName) -> list_to_atom(RoomName);
to_atom(RoomName) -> RoomName.

start(RoomName) ->
  process_flag(trap_exit, true),
  RoomAtom = to_atom(RoomName),
  register(RoomAtom, spawn(fun() -> loop(RoomAtom, []) end)).

loop(RoomName, Members) ->
  receive
    {join, User} ->
      io:format("~s joined ~s~n", [User, atom_to_list(RoomName)]),
      loop(RoomName, [User | Members]);

    {leave, User} ->
      io:format("~s left ~s~n", [User, atom_to_list(RoomName)]),
      loop(RoomName, lists:delete(User, Members));

    {message, User, Message} ->
      lists:foreach(fun(Member) ->
        io:format("Message to ~s from ~s: ~s~n", [Member, User, Message])
                    end, lists:delete(User, Members)),
      loop(RoomName, Members);

    {'EXIT', _Pid, _Reason} ->
      crash_recovery(RoomName)
  end.

join(RoomName, User) ->
  RoomAtom = to_atom(RoomName),
  RoomAtom ! {join, User}.

leave(RoomName, User) ->
  to_atom(RoomName) ! {leave, User}.

send_message(RoomName, {User, Message}) ->
  RoomAtom = to_atom(RoomName),
  RoomAtom ! {message, User, Message}.

crash_recovery(RoomName) ->
  io:format("Recovering chat room ~s~n", [RoomName]),
  start(RoomName).