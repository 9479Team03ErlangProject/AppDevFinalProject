%%%-------------------------------------------------------------------
%%% @author Kevin Roy Maglaqui
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module provides a simple in-memory user authentication system
%%%  using Erlang Term Storage (ETS). It includes functionalities for
%%%  starting and stopping the server, registering users, and authenticating them.
%%% @end
%%% Created : 01. Dec 2024 5:26 pm
%%%-------------------------------------------------------------------

-module(user_server).
-author("Maglaqui Kevin Roy").
-export([start/0, register_user/2, authenticate_user/2, stop/0]).


start() ->
  ets:new(users, [named_table, public]),
  {ok, "User server started"}.


stop() ->
  ets:delete(users),

  {ok, "User server stopped"}.


register_user(Username, Password) ->
  case ets:lookup(users, Username) of
    [] ->
      ets:insert(users, {Username, Password}),
      {ok, "User registered"};

    _ ->
      {error, "User already exists"}
  end.


authenticate_user(Username, Password) ->
  case ets:lookup(users, Username) of
    [{_, StoredPassword}] when StoredPassword == Password ->
      {ok, "Authenticated"};
    _ ->
      {error, "Invalid credentials"}
  end.