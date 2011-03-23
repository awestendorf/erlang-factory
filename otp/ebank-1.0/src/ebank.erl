-module(ebank).
-author("Aaron").

-export([start/2]).

start(_, [Name]) ->
  io:format("DEBUG: starting ebank ~p~n", [Name]),
  case supervisor:start_link({local, ebank}, ebank_sup, [Name]) of
    {ok, Pid} -> {ok, Pid, {}};
    Error -> Error
  end.

