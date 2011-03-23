-module(atm_sups).
-author("Aaron Westendorf").
-behavior(supervisor).
-vsn('0.1').

-export([start/1, start_link/1, init/1, stop/1]).

% Start a new atm
start(Name) ->
  supervisor:start({local,Name}, ?MODULE, [Name]).

start_link(Name) ->
  supervisor:start_link({local,Name}, ?MODULE, [Name]).

init([Name]) ->
  process_flag(trap_exit, true),
  { ok, 
    {{one_for_all, 3, 3000}, [
      {name(sw,Name), {atm, start_link, [Name]}, transient, 2000, worker, [atm] },
      {name(hw,Name), {atm_hw, start_link, [Name]}, transient, 2000, worker, [atm_hw] }
    ]}
  }.

stop(Name) ->
  exit(whereis(Name), shutdown).


% Generate a unique name
name(Domain, Name) ->
  list_to_atom("atm_" ++ atom_to_list(Domain) ++ "_" ++ atom_to_list(Name)).
