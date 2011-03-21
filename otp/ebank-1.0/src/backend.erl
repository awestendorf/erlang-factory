-module(backend).
-behavior(gen_server).
-include("../include/backend.hrl").

-export([start/0, start_link/0, stop/0, init/1,
	 account/1, pin_valid/2, change_pin/3,
	 balance/2, transactions/2,
	 withdraw/3,
	 transfer/4
	]).

-export([handle_call/3, handle_cast/2, terminate/2]).

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

terminate(Reason, Accounts) ->
  io:format("TERMINATING ~p~n", [Reason]),
  ok.

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
handle_call({account,Account}, _From, Accounts) ->
	case Account of
	  all ->
	    {reply, lists:map(
          fun(#account{no = No, name = Name}) -> {No, Name} end,
		      ?DB:db_to_list(Accounts)
        ), Accounts};
	  Name when is_list(Name) -> {reply, find_account(Name, Accounts), Accounts};
	  No when is_integer(No) -> {reply, [find_account(No, Accounts)], Accounts}
	end;

handle_call({pin_valid, AccountNumber, Pin}, _From, Accounts) ->
  Account = find_account(AccountNumber, Accounts),
  {reply, do_pin_valid(Account, Pin), Accounts};

handle_call({change_pin, User, OldPin, NewPin}, _From, Accounts) ->
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
    {error, ErrMsg} -> {reply, {error,ErrMsg}, Accounts};
    Balance -> {reply, Balance, Accounts}
  end;

handle_call({transactions, AccountNo, Pin}, _From, Accounts) ->
  case do_transactions(AccountNo, Pin, Accounts) of
    {error,ErrMsg} -> {reply, {error,ErrMsg}, Accounts};
    transactions -> {reply, transactions, Accounts}
  end;

handle_call({new_account, [Balance, Pin, Name]}, _From, Accounts) ->
  No = ?DB:db_size(Accounts) + 1,
  NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
  {reply, ok, NewAccounts};
    
handle_call({deposit, ToAccountN, Amount}, _From, Accounts) ->
  {ok, NewAccounts} = do_deposit(ToAccountN, Amount, Accounts),
  {reply, ok, NewAccounts}.

new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, Accounts) when is_integer(AccountN) ->
  ?DB:lookup(AccountN, Accounts);
find_account(User, Accounts) when is_list(User) ->
  ?DB:lookup_all(#account.name, User, Accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, Accounts) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, Accounts),
  case do_pin_valid(Account, Pin) of
    false -> {error, "PIN code not valid!"};
    true when OldBalance < Amount -> {error, "Not enough money on account!"};
    true ->
      NewBalance = OldBalance - Amount,
      NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
      AccountUpdated =
	Account#account{balance = NewBalance, transactions = NewTransactions},
      NewAccounts = ?DB:update(AccountUpdated, Accounts),
      {ok, NewAccounts}
  end.

do_deposit(AccountN, Amount, Accounts) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, Accounts),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
    Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, Accounts),
  {ok, NewAccounts}.

do_balance(AccountN, Pin, Accounts) ->
  Account = find_account(AccountN, Accounts),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.balance;
    false -> {error, "PIN code not valid!"}
  end.
	
do_transactions(AccountN, Pin, Accounts) ->
  Account = find_account(AccountN, Accounts),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

do_transfer(FromAccountN, ToAccountN, Pin, Amount, Accounts) ->
  case do_withdraw(FromAccountN, Pin, Amount, Accounts) of
    {ok, NewAccounts} -> do_deposit(ToAccountN, Amount, NewAccounts);
    {error, Reason} -> {error, Reason}
  end.

do_pin_valid([], _) -> false;
do_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
do_pin_valid(Account, Pin) -> Account#account.pin == Pin.

do_change_pin(User, OldPin, NewPin, Accounts) ->
  FoundAccounts = find_account(User, Accounts),
  case do_pin_valid(FoundAccounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      NewAccounts = lists:foldl(
        fun(Account, Acc) ->
          ?DB:update(Account#account{pin = NewPin}, Acc)
		    end,
		    Accounts,
		    FoundAccounts),
      {ok, NewAccounts}
  end.

