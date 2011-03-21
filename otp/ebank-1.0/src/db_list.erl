-module(db_list).
-author("Aaron Westendorf").

-include("../include/backend.hrl").

%-export([empty/0]).
-compile(export_all).

% For now just using a simple list.
% TODO: Change from list to a proper backend.

% Get a new database with no records in it
%empty() ->
%  mnesia:start(),
%  mnesia:create_table(account, [{attributes,record_info(fields,account)}] ).
empty() ->
  [].

% Insert an account into the database
% TODO: Handle when account exists.
insert(Account, DBRef) ->
  [Account | DBRef].

% List all of the accounts in the database.
db_to_list(DBRef) ->
  DBRef.

% Count the number of accounts in the database.
db_size(DBRef) ->
  length(DBRef).

% Lookup an account by its number. If no matches, return {
lookup(_AccountNumber, []) ->
  {error, instance};
lookup(AccountNumber, [Head|DBRef]) ->
  case Head#account.no =:= AccountNumber of
    true -> Head;
    false -> lookup(AccountNumber, DBRef)
  end.

% Lookup all accounts that match a given field.
% TODO: Switch to tail recursion
lookup_all(_AccountField, _Key, []) ->
  [];
lookup_all(AccountField, Key, [Head|DBRef]) ->
  case element(AccountField,Head) =:= Key of
    true -> [Head | lookup_all(AccountField,Key,DBRef)];
    false -> lookup_all(AccountField,Key,DBRef)
  end.

update(_Account, []) ->
  [];
update(Account, [Head,DBRef]) ->
  case Head#account.no =:= Account#account.no of
    true -> [Account | DBRef];
    false -> [Head | update(Account,DBRef)]
  end.
