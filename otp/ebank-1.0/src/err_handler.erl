-module(err_handler).
-author("Me").
-behavior(gen_event).

% Adds a log output handler for tracking when people do bad stuff.

-export([init/0, init/1, handle_event/2]).

init() ->
  gen_event:add_handler( alarm_handler, ?MODULE, [] ).

init(_Args) ->
  error_logger:logfile({open, "errors"}),
  {ok, []}.

% log stolen accounts
handle_event({account_stolen, {Atm, AccountNumber}}, State) ->
  % TODO: send to error_logger which is a simple thing
  io:format("DEBUG: account stolen!~n"),
  {ok, State};

% ignore everything else
handle_event(Event, State) ->
  {ok, State}.
