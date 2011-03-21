-module(backend).
-behavior(gen_server).
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

%start() -> spawn(?MODULE, init, []).
start() ->
  start_link().

start_link() -> 
  gen_server:start_link({local,backend}, ?MODULE, [], []).

% Callback from gen_server start
init(_Args) ->
  process_flag(trap_exit, true),
  % Not needed because this is gen server? register(?MODULE, self()),
  Accounts =
    lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
		    ?DB:insert(new_account(No, Balance, Pin, Name), DB)
		end,
		?DB:empty(),
		?ACCOUNTS),
  {ok, Accounts}.

stop() ->
  gen_server:cast(backend, stop).

account(Account) -> 
  gen_server:call(backend, {account, Account}).

pin_valid(AccountNo, Input) -> 
  gen_server:call(backend, {pin_valid, AccountNo, Input}).

change_pin(User, OldPin, NewPin) -> 
  gen_server:call(backend, {change_pin, User, OldPin, NewPin}).

withdraw(AccountNo, Pin, Ammount) -> 
  gen_server:call(backend, {withdraw, AccountNo, Pin, Ammount}).

transfer(Ammount, From, To, Pin) -> 
  gen_server:call(backend, {transfer, From, To, Pin, Ammount}).

balance(AccountNo, Pin) -> 
  gen_server:call(backend, {balance, AccountNo, Pin}).

transactions(AccountNo, Pin) -> 
  gen_server:call(backend, {transactions, AccountNo, Pin}).

% Handle stop call asynchronously
handle_cast(stop, State) ->
  {stop, normal, State}.

% Synchronous calls

% Search for a specific account
handle_call({account,Account), _From, Accounts) ->
	case Account of
	  all ->
	    {reply, lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
		      ?DB:db_to_list(State#state.accounts)), Accounts};
	  Name when list(Name) -> {reply, find_account(Name, State), Accounts};
	  No when integer(No) -> {reply, [find_account(No, State)], Accounts}
	end;

handle_call({pin_valid, AccountNo, Pin}, _From, Accounts) ->
  {reply, find_account(AccountNo,Pin), Accounts};

handle_call({change_ping, User, OldPin, NewPin}, _From, Accounts) ->
  case do_change_pin(User, OldPin, NewPin, Accounts) of
    {error, ErrMsg} -> {reply, {error, ErrMsg}, Accounts};
    {ok, NewAccounts} -> {reply, ok, NewAccounts}
  end;

handle_call({withdraw, AccountNo, Pin, Ammount}, _From, Accounts) ->
  case do_withdraw(AccountNo, Pin, Ammount, Accounts) of
    {error, ErrMsg} -> {reply, {error, ErrMsg}, Accounts};
    {ok, NewAccounts} -> {reply, ok, NewAccounts}
  end;

handle_call({transfer, From, To, Pin, Ammount}, _From, Accounts) ->
  case do_transfer(From, To, Pin, Ammount, Accounts) of
    {error, ErrMsg} -> {reply, {error,ErrMsg}, Accounts};
    {ok, NewAccounts} -> {reply, ok, NewAccounts}
  end;
  
handle_call({balance, AccountNo, Pin}, _From, Accounts) ->
  case do_balance(AccountNo, Pin, Accounts) of
    {error, ErrMsg} -> {reply, {error,ErrorMsg}, Accounts};
    balance -> {reply, balance, Accounts}
  end;

handle_call({transactions, AccountNo, Pin}) ->
  case do_transactions(AccountNo, Pin, Accounts) of
    {error,ErrorMsg} -> {reply, {error,ErrorMsg}, Accounts};
    transactions -> {reply, transactions, Accounts}
  end.


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

