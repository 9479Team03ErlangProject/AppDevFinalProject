
%%%-------------------------------------------------------------------
%%% @author Lei Ceasar Bagsan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module implements a high-level interface for a chat application.
%%%  It integrates user authentication and chat room functionalities.
%%% @end
%%% Created : 01. Dec 2024 5:27 pm
%%%-------------------------------------------------------------------


-module(chat_app).
-author("Lei Ceasar Bagsan").
-export([start/0, register_user/2, login/2, join_room/2, send_message/3]).




% Starts the chat application by initializing the user server.
start() ->
  user_server:start(),
  io:format("Chat application started.~n").


% Registers a new user by delegating the request to the user_server module.
register_user(Username, Password) ->
  user_server:register_user(Username, Password).


% Authenticates a user using the user_server module.
login(Username, Password) ->
  case user_server:authenticate_user(Username, Password) of
    {ok, _} ->
      io:format("~s logged in successfully.~n", [Username]),
      {ok, Username};
    {error, Reason} ->
      io:format("Login failed: ~s~n", [Reason]),
      {error, Reason}
  end.


% Allows a user to join a chat room.
join_room(Username, RoomName) ->
  chat_room:start(RoomName),
  chat_room:join(RoomName, Username).




% Sends a message from a user to a chat room.
send_message(Username, RoomName, Message) ->
  chat_room:send_message(RoomName, {Username, Message}).
