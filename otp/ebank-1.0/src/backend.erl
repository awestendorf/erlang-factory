-module(backend).
-behavior(gen_server).
-include("../include/backend.hrl").

-export([start/0, start_link/0, stop/0, init/1,
	 account/1, pin_valid/2, change_pin/3,
	 balance/2, transactions/2,
	 withdraw/3,
	 transfer/4,
   block/1
	]).

-export([handle_call/3, handle_cast/2, terminate/2]).

-define(DB, db_list).
-define(ACCOUNTS,
	[{1, 100, "1234", "Henry Nystrom"},
	 {2, 200, "4321", "Francesco Cesarini"},
	 {3, 1000, "1111", "Donald Duck"},
	 {4, 5000, "1234", "Henry Nystrom"}
	]).

-record(state, {accounts,
    blocked=[]
  }).

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
  {ok, #state{accounts=Accounts}}.

stop() ->
  gen_server:cast(backend, stop).

terminate(Reason, State) ->
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

block(AccountNo) ->
  gen_server:call(backend, {block, AccountNo}).

% Handle stop call asynchronously
handle_cast(stop, State) ->
  {stop, normal, State}.

% Synchronous calls

% Search for a specific account
handle_call({account,Account}, _From, State) ->
	case Account of
	  all ->
	    {reply, lists:map(
          fun(#account{no = No, name = Name}) -> {No, Name} end,
		      ?DB:db_to_list(State#state.accounts)
        ), State};
	  Name when is_list(Name) -> {reply, find_account(Name, State), State};
	  No when is_integer(No) -> {reply, [find_account(No, State)], State}
	end;

handle_call({pin_valid, AccountNumber, Pin}, _From, State) ->
  Account = find_account(AccountNumber, State),
  {reply, do_pin_valid(Account, Pin), State};

handle_call({change_pin, User, OldPin, NewPin}, _From, State) ->
  case do_change_pin(User, OldPin, NewPin, State) of
    {error, ErrMsg} -> {reply, {error, ErrMsg}, State};
    {ok, NewState} -> {reply, ok, NewState}
  end;

handle_call({withdraw, AccountNo, Pin, Ammount}, _From, State) ->
  case do_withdraw(AccountNo, Pin, Ammount, State) of
    {error, ErrMsg} -> {reply, {error, ErrMsg}, State};
    {ok, NewState} -> {reply, ok, NewState}
  end;

handle_call({transfer, From, To, Pin, Ammount}, _From, State) ->
  case do_transfer(From, To, Pin, Ammount, State) of
    {error, ErrMsg} -> {reply, {error,ErrMsg}, State};
    {ok, NewState} -> {reply, ok, NewState}
  end;
  
handle_call({balance, AccountNo, Pin}, _From, State) ->
  case do_balance(AccountNo, Pin, State) of
    {error, ErrMsg} -> {reply, {error,ErrMsg}, State};
    Balance -> {reply, Balance, State}
  end;

handle_call({transactions, AccountNo, Pin}, _From, State) ->
  case do_transactions(AccountNo, Pin, State) of
    {error,ErrMsg} -> {reply, {error,ErrMsg}, State};
    Transactions -> {reply, Transactions, State}
  end;

handle_call({new_account, [Balance, Pin, Name]}, _From, State) ->
  No = ?DB:db_size(State) + 1,
  NewState = ?DB:insert(new_account(No, Balance, Pin, Name), State),
  {reply, ok, NewState};
    
handle_call({deposit, ToAccountN, Amount}, _From, State) ->
  {ok, NewState} = do_deposit(ToAccountN, Amount, State),
  {reply, ok, NewState};

handle_call({block, AccountNo}, _From, State) ->
  case lists:member(AccountNo, State#state.blocked) of
    false -> {reply, ok, State#state{blocked = [AccountNo|State#state.blocked]} };
    _ -> {reply, ok, State}
  end.

new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, State) when is_integer(AccountN) ->
  ?DB:lookup(AccountN, State#state.accounts);
find_account(User, State) when is_list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    false -> 
      {error, "PIN code not valid!"};
    true when OldBalance < Amount -> 
      {error, "Not enough money on account!"};
    true ->
      case lists:member(AccountN, State#state.blocked) of
        true -> {error, "Not enough money on account!"};
        false ->
          NewBalance = OldBalance - Amount,
          NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
          AccountUpdated =
            Account#account{balance = NewBalance, transactions = NewTransactions},
          NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
          {ok, State#state{accounts = NewAccounts} }
      end
  end.

do_deposit(AccountN, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
    Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts} }.

do_balance(AccountNo, Pin, State) ->
  Account = find_account(AccountNo, State),
  case do_pin_valid(Account, Pin) of
    true -> 
      case lists:member(AccountNo, State#state.blocked) of
        true -> 0;
        _ -> Account#account.balance
      end;
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
      NewAccounts = lists:foldl(
        fun(Account, Acc) ->
          ?DB:update(Account#account{pin = NewPin}, Acc)
		    end,
		    State#state.accounts,
		    Accounts),
      {ok, State#state{accounts = NewAccounts} }
  end.

