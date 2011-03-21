-module(atm).

-export([start/1, start_link/1, stop/1, init/1,
	 card_inserted/2, event/2]).

-record(state, {name, accountNo, input = [], pin}).


start(Name) -> spawn(?MODULE, init, [Name]).

start_link(Name) -> {ok, spawn_link(?MODULE, init, [Name])}.

send_event(Event, Name) -> name(Name) ! Event.


stop(Name) -> send_event(stop, Name).

event(E, Name) -> send_event(E, Name).

card_inserted(Account, Name) -> send_event({card_inserted, Account}, Name).

init(Name) ->
 register(name(Name), self()),
  idle(#state{name = Name}).

idle(State = #state{name = Name}) ->
  receive
    {card_inserted, AccountNumber} ->
      atm_hw:display("\n\n   Please type your PIN code", Name),
      get_pin(#state{name = Name, accountNo = AccountNumber});
    stop -> normal;
    _ -> idle(State)
  end.

get_pin(State = #state{name = Name, accountNo = AccountNo, input = Input}) ->
  receive
    clear -> clear(get_pin, State);
    cancel -> cancel(State);
    {digit, Digit} ->
      Digits = State#state.input ++ Digit,
      atm_hw:display(Digits, State#state.name),
      get_pin(State#state{input = Digits});
    enter ->
      case backend:pin_valid(AccountNo, Input) of
	true ->
	  atm_hw:display("\n\n   Please make your selection", Name),
	  selection(State#state{pin = Input, input = []});
	false ->
	  atm_hw:display("\n\n    PIN code incorrect!\n    Please try again.",
			 Name),
	  get_pin(State#state{input = []})
      end;
    {selection, _} -> get_pin(State);
    stop -> normal
  end.

selection(State = #state{name = Name}) ->
  receive
    clear -> clear(selection, State);
    cancel -> cancel(State);
    {selection, select1} ->
      atm_hw:high_light(select1, Name),
      atm_hw:display("\n\n    How much would you like\n    to withdraw?",
		     Name),
      withdraw(State);
    {selection, select2} ->
      atm_hw:high_light(select2, Name),
      atm_hw:display(balance(State), Name),
      selection(State);
    {selection, select3} ->
      atm_hw:high_light(select3, Name),
      atm_hw:display(mini_statement(State), Name),
      selection(State);
    {digit, _} -> selection(State);
    enter -> selection(State);
    stop -> normal
  end.

withdraw(State = #state{name=Name, accountNo=AccNo, pin=Pin, input=Input}) ->
  receive  
    clear -> clear(withdraw, State);
    cancel -> cancel(State);
    {digit, Digit} ->
      Digits = State#state.input ++ Digit,
      atm_hw:display(Digits, State#state.name),
      withdraw(State#state{input = Digits});
    enter -> 
      Input1 = list_to_integer(Input),
      case backend:withdraw(AccNo, Pin, Input1) of
	ok ->
	  atm_hw:display("\n\n    Take the money and run.", Name),
	  receive after 3500 -> ok end,
	  atm_hw:high_light(off, Name),
	  atm_hw:eject(Name);
	{error, Reason} ->
	  Str = io_lib:format("\nCould not withdraw money!\n~p.", [Reason]),
	  atm_hw:display(Str, Name),
	  receive after 3500 -> ok end,
	  atm_hw:high_light(off, Name),
	  atm_hw:eject(Name)
      end,
      idle(#state{name = Name});
    {selection, _} -> withdraw(State);
    stop -> normal
  end.

clear(StateName, State) ->
  atm_hw:display("", State#state.name),
  StateName(State#state{input = []}).
  
cancel(#state{name = Name}) ->
  atm_hw:display("cancel: Cancel button pressed\n", Name),
  atm_hw:eject(Name),  
  idle(#state{name = Name}).

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

name(Name) -> list_to_atom("atm_" ++ atom_to_list(Name)).
