-module(backend).
-include("../include/backend.hrl").
-export([start/0, start_link/0, stop/0, init/0,
	 account/1, pin_valid/2, change_pin/3,
	 balance/2, transactions/2,
	 withdraw/3,
	 transfer/4
	]).

-define(DB, db_list).
-define(ACCOUNTS,
	[{1, 100, "1234", "Henry Nystrom"},
	 {2, 200, "4321", "Francesco Cesarini"},
	 {3, 1000, "1111", "Donald Duck"},
	 {4, 5000, "1234", "Henry Nystrom"}
	]).

-record(state, {accounts}).

start() -> spawn(?MODULE, init, []).

start_link() -> {ok, spawn_link(?MODULE, init, [])}.


stop() -> ?MODULE ! stop.

account(Account) -> call({account, Account}).

pin_valid(AccountNo, Input) -> call({pin_valid, AccountNo, Input}).

change_pin(User, OldPin, NewPin) -> call({change_pin, User, OldPin, NewPin}).


withdraw(AccountNo, Pin, Ammount) -> call({withdraw, AccountNo, Pin, Ammount}).


transfer(Ammount, From, To, Pin) -> call({transfer, From, To, Pin, Ammount}).


balance(AccountNo, Pin) -> call({balance, AccountNo, Pin}).

transactions(AccountNo, Pin) -> call({transactions, AccountNo, Pin}).

call(X) ->
  ?MODULE ! {X, self()},
  receive {?MODULE, reply, R} -> R end.

reply(To, X) -> To ! {?MODULE, reply, X}.

init() ->
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  Accounts =
    lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
		    ?DB:insert(new_account(No, Balance, Pin, Name), DB)
		end,
		?DB:empty(),
		?ACCOUNTS),
  loop(#state{accounts = Accounts}).

loop(State) ->
  receive 
    {{account, Accounts}, From} ->
      Reply =
	case Accounts of
	  all ->
	    lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
		      ?DB:db_to_list(State#state.accounts));
	  Name when list(Name) -> find_account(Name, State);
	  No when integer(No) -> [find_account(No, State)]
	end,
      reply(From, Reply),
      loop(State);
    {{pin_valid, AccountNumber, Pin}, From} ->
      Account = find_account(AccountNumber, State),
      reply(From, do_pin_valid(Account, Pin)),
      loop(State);
    {{new_account, [Balance, Pin, Name]}, From} ->
      Accounts = State#state.accounts,
      No = ?DB:db_size(Accounts) + 1,
      NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
      reply(From, ok),
      loop(State#state{accounts = NewAccounts});
    {{balance, AccountN, Pin}, From} ->
      reply(From, do_balance(AccountN, Pin, State)),
      loop(State);
    {{transactions, AccountN, Pin}, From} ->
      reply(From, do_transactions(AccountN, Pin, State)),
      loop(State);
    {{withdraw, FromAccountN, Pin, Amount}, From} ->
      case do_withdraw(FromAccountN, Pin, Amount, State) of
	{ok, NewState} -> reply(From, ok), loop(NewState);
	{error, Reason} -> reply(From, {error, Reason}), loop(State)
      end;
    {{deposit, ToAccountN, Amount}, From} ->
      case do_deposit(ToAccountN, Amount, State) of
	{ok, NewState} -> reply(From, ok), loop(NewState);
	{error, Reason} -> reply(From, {error, Reason}), loop(State)
      end;
    {{transfer, FromAccountN, ToAccountN, Pin, Amount}, From} ->
      case do_transfer(FromAccountN, ToAccountN, Pin, Amount, State) of
	{ok, NewState} -> reply(From, ok), loop(NewState);
	{error, Reason} -> reply(From, {error, Reason}), loop(State)
      end;
    {{change_pin, User, OldPin, NewPin}, From} ->
      case do_change_pin(User, OldPin, NewPin, State) of
	{ok, NewState} -> reply(From, ok), loop(NewState);
	{error, Reason} -> reply(From, {error, Reason}), loop(State)
      end;
    stop -> normal
  end.

new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, State) when integer(AccountN) ->
  ?DB:lookup(AccountN, State#state.accounts);
find_account(User, State) when list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    false -> {error, "PIN code not valid!"};
    true when OldBalance < Amount -> {error, "Not enough money on account!"};
    true ->
      NewBalance = OldBalance - Amount,
      NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
      AccountUpdated =
	Account#account{balance = NewBalance, transactions = NewTransactions},
      NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
      {ok, State#state{accounts = NewAccounts}}
  end.

do_deposit(AccountN, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
    Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts}}.

do_balance(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.balance;
    false -> {error, "PIN code not valid!"}
  end.
	
do_transactions(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

do_transfer(FromAccountN, ToAccountN, Pin, Amount, State) ->
  case do_withdraw(FromAccountN, Pin, Amount, State) of
    {ok, NewState} -> do_deposit(ToAccountN, Amount, NewState);
    {error, Reason} -> {error, Reason}
  end.

do_pin_valid([], _) -> false;
do_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
do_pin_valid(Account, Pin) -> Account#account.pin == Pin.

do_change_pin(User, OldPin, NewPin, State) ->
  Accounts = find_account(User, State),
  case do_pin_valid(Accounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      Accounts1 =
	lists:foldl(fun(Account, Acc) ->
			?DB:update(Account#account{pin = NewPin}, Acc)
		    end,
		    State#state.accounts,
		    Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.

