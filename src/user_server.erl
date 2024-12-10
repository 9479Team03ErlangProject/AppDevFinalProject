%%%-------------------------------------------------------------------
%%% @author Maglaqui Kevin Roy
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module provides a simple in-memory user authentication system
%%%  using Erlang Term Storage (ETS). It includes functionalities for
%%%  starting and stopping the server, registering users, and authenticating them.
%%% @end
%%% Created : 01. Dec 2024 5:26 pm
%%%-------------------------------------------------------------------

-module(user_server).
-author("Maglaqui Kevin").
-export([start/0, register_user/2, authenticate_user/2, stop/0]).

%% Starts the user server by creating an ETS table named 'users'.
start() ->
  ets:new(users, [named_table, public]),
  {ok, "User  server started"}.

%% Stops the user server by deleting the ETS table named 'users'.
stop() ->
  ets:delete(users),
  {ok, "User  server stopped"}.

%% Registers a user with a given Username and Password.
register_user(Username, Password) ->
  case ets:lookup(users, Username) of
    [] ->
      ets:insert(users, {Username, Password}),
      {ok, "User  registered"};
    _ ->
      {error, "User  already exists"}
  end.

%% Authenticates a user by checking the given Username and Password.
authenticate_user(Username, Password) ->
  case ets:lookup(users, Username) of
    [{_, StoredPassword}] when StoredPassword == Password ->
      {ok, "Authenticated"};
    _ ->
      {error, "Invalid credentials"}
  end.