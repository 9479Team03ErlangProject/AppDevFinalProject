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

start(RoomName) ->
  process_flag(trap_exit, true),
  % Convert RoomName (which is a string) to an atom
  RoomAtom = list_to_atom(RoomName),
  register(RoomAtom, spawn(fun() -> loop(RoomAtom, []) end)).

loop(RoomName, Members) ->
  receive
    {join, User} ->
      io:format("~s joined ~s~n", [User, RoomName]),
      loop(RoomName, [User | Members]);

    {leave, User} ->
      io:format("~s left ~s~n", [User, RoomName]),
      loop(RoomName, lists:delete(User, Members));

    {message, User, Message} ->
      lists:foreach(fun(Member) ->
        if Member /= User ->
          io:format("Message to ~s from ~s: ~s~n", [Member, User, Message]);
          true -> ok
        end
                    end, Members),
      loop(RoomName, Members);

    {'EXIT', _Pid, _Reason} ->
      crash_recovery(RoomName)
  end.

join(RoomName, User) ->
  % Convert RoomName to atom if it is a string
  RoomAtom = list_to_atom(RoomName),
  RoomAtom ! {join, User}.
leave(RoomName, User) -> RoomName ! {leave, User}.
send_message(RoomName, {User, Message}) ->
  % Convert RoomName to atom if it is a string
  RoomAtom = list_to_atom(RoomName),
  RoomAtom ! {message, User, Message}.
crash_recovery(RoomName) ->
  io:format("Recovering chat room ~s~n", [RoomName]),
  start(RoomName).