-module(atm).
-behavior(gen_fsm).
-vsn('0.1').

-export([start/1, start_link/1, stop/1, init/1,
	 card_inserted/2, event/2]).

-export([wait_for_card/2, wait_for_pin/2, wait_for_selection/2, withdraw/2]).

-export([handle_sync_event/4, terminate/3]).

-record(state, {name, accountNo, input = [], pin}).

-define(TIMEOUT, 10000).


start(Name) -> 
  start_link(Name).

start_link(Name) -> 
  gen_fsm:start_link({local,name(Name)}, ?MODULE, Name, []).

% For now send synchronous for all states
stop(Name) -> 
  gen_fsm:sync_send_all_state_event(name(Name), stop).

% Handle all sync events for stop
handle_sync_event(stop, _From, State, LoopData) ->
  {stop, normal, State, LoopData}.

terminate(Reason, StateName, State) ->
  io:format("Terminated at ~p with state ~p due to ~p~n", [StateName, State, Reason]),
  ok.

% Callback from hardware, will queue through fsm. E is the event name, Name is
% the same name passed into start() and atm_hw:start().
event(E, Name) -> 
  gen_fsm:send_event(name(Name), E).

card_inserted(Account, Name) -> 
  gen_fsm:send_event(name(Name), {card_inserted, Account}).

init(Name) ->
  {_,_,State} = cancel( #state{name=Name} ),
  {ok, wait_for_card, State}.

%%
%%  Idle state, wait for a card
%%
wait_for_card({card_inserted, AccountNumber}, State) ->
  case backend:blocked(AccountNumber) of
    true -> 
      % Technically, we should eat the card and call the cops, but this works too
      alarm_handler:set_alarm({account_stolen, {State#state.name,AccountNumber}}),
      atm_hw:display("\n\n   ZOMG YOU STOLE IT!", State#state.name),
      timer:sleep(2000),
      cancel(State);
    false ->
      atm_hw:display("\n\n   Please type your PIN code", State#state.name),
      {next_state, wait_for_pin, State#state{accountNo=AccountNumber}, ?TIMEOUT }
  end;

wait_for_card(_Ignored, State) ->
  {next_state, wait_for_card, State}.

%%
%% Card inserted, wait for the pin
%%

wait_for_pin({digit, Digit}, State) ->
  Digits = State#state.input ++ Digit,
  atm_hw:display(Digits, State#state.name),
  {next_state, wait_for_pin, State#state{input=Digits}, ?TIMEOUT};

wait_for_pin(cancel, State) ->
  cancel(State);

wait_for_pin(clear, State) ->
  clear(wait_for_pin, State);

wait_for_pin(enter, State) ->
  case backend:pin_valid(State#state.accountNo, State#state.input) of
    true -> 
      atm_hw:display("\n\n   Please make your selection", State#state.name),
      {next_state, wait_for_selection, State#state{pin=State#state.input, input=[]}, ?TIMEOUT };
    false ->
      atm_hw:display("\n\n   PIN code incorrect!\n    Please try again."),
      {next_state, wait_for_pin, State#state{input=[]}, ?TIMEOUT}
  end;

wait_for_pin(timeout, State) ->
  cancel(State);

% Trap everything else and ignore
wait_for_pin(_Ignored, State) ->
  {next_state, wait_for_pin, State}.

%%
%% Wait for a selection
%%
wait_for_selection(clear, State) ->
  clear(wait_for_selection, State);

wait_for_selection(cancel, State) ->
  cancel(State);

wait_for_selection({selection,select1}, State) ->
  atm_hw:high_light(select1, State#state.name),
  atm_hw:display("\n\n    How much would you like\n    to withdraw?",
		     State#state.name),
  {next_state, withdraw, State, ?TIMEOUT};

wait_for_selection({selection,select2}, State) ->
  atm_hw:high_light(select2, State#state.name),
  atm_hw:display(balance(State), State#state.name),
  {next_state, wait_for_selection, State, ?TIMEOUT};

wait_for_selection({selection,select3}, State) ->
  atm_hw:high_light(select3, State#state.name),
  atm_hw:display(mini_statement(State), State#state.name),
  {next_state, wait_for_selection, State, ?TIMEOUT};

wait_for_selection(timeout, State) ->
  cancel(State);

% Ignore everthing else
wait_for_selection(_Ignored, State) ->
  {next_state, wait_for_selection, State}.


%%
%% Withdraw some moneys
%%
withdraw(clear, State) ->
  clear(withdraw, State);
withdraw(cancel, State) ->
  cancel(State);

withdraw({digit, Digit}, State) ->
  Digits = State#state.input ++ Digit,
  atm_hw:display(Digits, State#state.name),
  {next_state, withdraw, State#state{input=Digits}, ?TIMEOUT};

withdraw(enter, State) ->
  Amount = list_to_integer(State#state.input),
  case backend:withdraw(State#state.accountNo, State#state.pin, Amount) of
    ok ->
      atm_hw:display("\n\n    Take the money and run.", State#state.name),
      timer:sleep(3500),
      atm_hw:high_light(off, State#state.name),
      eject(State),
      {next_state, wait_for_card, #state{name=State#state.name}};
    {error, Reason} ->
	    Msg = io_lib:format("\nCould not withdraw money!\n~p.", [Reason]),
	    atm_hw:display(Msg, State#state.name),
      timer:sleep(3500),
      atm_hw:high_light(off, State#state.name),
      eject(State),
      {next_state, wait_for_card, #state{name=State#state.name}}
  end;

withdraw(timeout, State) ->
  cancel(State);

% Ignore everything else
withdraw(_Ignored, State) ->
  {next_state, withdraw, State}.

clear(StateName, State) ->
  atm_hw:display("", State#state.name),
  {next_state, StateName, State#state{input=[]}, ?TIMEOUT}.
  %StateName(State#state{input = []}).

eject(State) ->
  atm_hw:eject(State#state.name),
  alarm_handler:clear_alarm(account_stolen),
  ok.

% Cancels the current action and returns the wait_for_card state
% TODO: what should the account number be reset to?
cancel(State) ->
  eject(State),
  {next_state, wait_for_card, State#state{input=[], accountNo=0}}.

balance(#state{accountNo = No, pin = Pin}) ->
  io_lib:format("\n\n   Balance:\n-------------------------\n   \243 ~p",
		[backend:balance(No, Pin)]).

mini_statement(#state{accountNo = No, pin = Pin}) ->
  Trs = backend:transactions(No, Pin),
  Balance = backend:balance(No, Pin),
  Trs1 = select10(Trs, [], 9),
  Head = "\n\n   Mini Statement.\n-------------------------\n",
  Trs2 = 
    lists:append(
      lists:map(fun({Type, {Year, Month, Day}, Sum}) ->
		    Con = case Type of
			    deposit -> "";
			    withdraw -> "-"
			  end,
		    io_lib:format("~p/~p/~p ~s ~p~n",
				  [Day, Month, Year, Con, Sum])
		end,
		Trs1)),
  Bal = io_lib:format("\n   Balance: \243 ~p", [Balance]),
  Head ++ Trs2 ++ Bal.

select10([], Acc, _) -> Acc;
select10(_, Acc, 0) -> Acc;
select10([H | T], Acc, N) -> select10(T, [H | Acc], N - 1).

% Create a local process name
name(Name) -> list_to_atom("atm_sw_" ++ atom_to_list(Name)).
