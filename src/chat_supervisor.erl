%%%-------------------------------------------------------------------
%%% @author Aragon, Danielle John
%%% @copyright (C) 2024, <AppDevFinal>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2024 6:46 pm
%%%-------------------------------------------------------------------
-module(chat_supervisor).
-author("Danielle Aragon").
-export([start_link/0, init/1]).

% Starts the supervisor process.
start_link() ->
  supervisor:start_link({local, chat_supervisor}, ?MODULE, []).

% Initializes the supervisor and defines the child processes.
init([]) ->
  {ok, {{one_for_one, 5, 10}, [{chat_room, {chat_room, start, ["Room1"]}, permanent, 5000, worker, [chat_room]}]}}.
