-module(atm).
-behavior(gen_fsm).
-vsn('0.1').

-export([start/1, start_link/1, stop/1, init/1,
	 card_inserted/2, event/2]).

-export([wait_for_card/2, wait_for_pin/2, wait_for_selection/2, withdraw/2]).

-export([handle_sync_event/4, terminate/3]).

-record(state, {name, accountNo, input = [], pin}).


start(Name) -> 
  start_link(Name).

start_link(Name) -> 
  gen_fsm:start_link({local,Name}, ?MODULE, Name, []).

%send_event(Event, Name) -> name(Name) ! Event.

% For now send synchronous for all states
stop(Name) -> 
  gen_fsm:sync_send_all_state_event(Name, stop).
  %send_event(stop, Name).

% Handle all sync events for stop
handle_sync_event(stop, _From, State, LoopData) ->
  {stop, normal, State, LoopData}.

terminate(Reason, StateName, State) ->
  io:format("Terminated at ~p with state ~p due to ~p~n", [StateName, State, Reason]).

% Callback from hardware, will queue through fsm. E is the event name, Name is
% the same name passed into start() and atm_hw:start().
event(E, Name) -> 
  io:format("Sending event ~p~n", [E]),
  gen_fsm:send_event(Name, E).
  %send_event(E, Name).

card_inserted(Account, Name) -> 
  gen_fsm:send_event(Name, {card_inserted, Account}).
  %send_event({card_inserted, Account}, Name).


%init(Name) ->
% register(name(Name), self()),
%  idle(#state{name = Name}).

% TODO: abstract the initialization message
init(Name) ->
  %atm_hw:display("\n\n   Please insert your awesome new card with moneys", Name),
  %{next_state, wait_for_card, "foooooooo"}.
  %{ok, wait_for_card, #state{name = Name}}.
  {_,_,State} = cancel( #state{name=Name} ),
  {ok, wait_for_card, State}.

%%
%%  Idle state, wait for a card
%%
wait_for_card({card_inserted, AccountNumber}, State) ->
  atm_hw:display("\n\n   Please type your PIN code", State#state.name),
  {next_state, wait_for_pin, State#state{accountNo=AccountNumber} }.


%%
%% Card inserted, wait for the pin
%%

wait_for_pin({digit, Digit}, State) ->
  Digits = State#state.input ++ Digit,
  atm_hw:display(Digits, State#state.name),
  {next_state, wait_for_pin, State#state{input=Digits}};

wait_for_pin(cancel, State) ->
  cancel(State);

wait_for_pin(clear, State) ->
  clear(wait_for_pin, State);

wait_for_pin(enter, State) ->
  case backend:pin_valid(State#state.accountNo, State#state.input) of
    true -> 
      atm_hw:display("\n\n   Please make your selection", State#state.name),
      {next_state, wait_for_selection, State#state{pin=State#state.input, input=[]} };
    false ->
      atm_hw:display("\n\n   PIN code incorrect!\n    Please try again."),
      {next_state, wait_for_pin, State#state{input=[]}}
  end;

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
  {next_state, withdraw, State};

wait_for_selection({selection,select2}, State) ->
  atm_hw:high_light(select2, State#state.name),
  atm_hw:display(balance(State), State#state.name),
  {next_state, wait_for_selection, State};

wait_for_selection({selection,select3}, State) ->
  atm_hw:high_light(select3, State#state.name),
  atm_hw:display(mini_statement(State), State#state.name),
  {next_state, wait_for_selection, State};

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
  {next_state, withdraw, State#state{input=Digits}};

withdraw(enter, State) ->
  Amount = list_to_integer(State#state.input),
  case backend:withdraw(State#state.accountNo, State#state.pin, Amount) of
    ok ->
      atm_hw:display("\n\n    Take the money and run.", State#state.name),
      timer:sleep(3500),
      atm_hw:high_light(off, State#state.name),
      atm_hw:eject(State#state.name),
      {next_state, wait_for_card, #state{name=State#state.name}};
    {error, Reason} ->
	    Msg = io_lib:format("\nCould not withdraw money!\n~p.", [Reason]),
	    atm_hw:display(Msg, State#state.name),
      timer:sleep(3500),
      atm_hw:high_light(off, State#state.name),
      atm_hw:eject(State#state.name),
      {next_state, wait_for_card, #state{name=State#state.name}}
  end;

% Ignore everything else
withdraw(_Ignored, State) ->
  {next_state, withdraw, State}.

clear(StateName, State) ->
  atm_hw:display("", State#state.name),
  {next_state, StateName, State#state{input=[]}}.
  %StateName(State#state{input = []}).

% Cancels the current action and returns the wait_for_card state
% TODO: what should the account number be reset to?
cancel(State) ->
  %atm_hw:display("cancel: Cancel button pressed\n", Name),
  %atm_hw:display("\n\n   Please insert your awesome new card with moneys", Name),
  atm_hw:eject(State#state.name),
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

%name(Name) -> list_to_atom("atm_" ++ atom_to_list(Name)).
