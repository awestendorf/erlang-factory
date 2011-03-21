%%%----------------------------------------------------------------------
%%% File    : gen_gs_server.erl
%%% Author  : Henry Nystrom <jann@erlang-consulting.com>
%%% Purpose : A generic server behaviour that deals with gs messages.
%%% Created : 2 July 2006 by Jan Nystrom <jann@erlang-consulting.com>
%%% Copyright (C) 2006 Erlang Consulting Ltd (www.erlang-consulting.com)
%%%----------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(gen_gs_server).
-version('1.0').
-behaviour(gen_server).
-author('jann@erlang-consulting.com').
-copyright('Copyright (c) 2004 Erlang Consulting Ltd (www.erlang-consulting.com)').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
% Gen server part
%
-export([start/3, start/4, start_link/3, start_link/4,
	 call/2, call/3, multi_call/2, multi_call/3, multi_call/4,
	 cast/2, reply/2, abcast/2, abcast/3
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS/GEN_SERVER CALLBACK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour DEF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},		% The gen server part
     {handle_call,3},
     {handle_cast,2},
     {handle_info,2},
     {terminate,2},
     {code_change,3},
     {handle_gs, 5}	% The gen gs part
    ];
behaviour_info(_Other) ->
    undefined.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @type server_state() = {State::term(), Module::atom()}
%%
%% @type registration() = {atom(local), Name::atom()} | 
%%                        {atom(global), Name::term()}
%%
%% @type client() = {pid(), Tag::reference()}
%%
%% @type server_ref() = atom() | pid()
-record(server_state, {state, module}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec start(Module::atom(), [term()], [term()]) -> 
%%         {atom(ok), pid()} | {error, Reason::term()}
%
start(Mod, Args, Options) ->
  gen_server:start(?MODULE, {init, Mod, Args}, Options).

%%%%%
%% @spec start(registration(), Module::atom(), [term()], [term()]) -> 
%%         {atom(ok), pid()} | {error, Reason::term()}
%
start(Name, Mod, Args, Options) ->
  gen_server:start(Name, ?MODULE, {init, Mod, Args}, Options).

%%%%%
%% @spec start_link(Module::atom(), [term()], [term()]) -> 
%%         {atom(ok), pid()} | {error, Reason::term()}
%
start_link(Mod, Args, Options) ->
  gen_server:start_link(?MODULE, {init, Mod, Args}, Options).

%%%%%
%% @spec start_link(Module::atom(), [term()], [term()]) -> 
%%         {atom(ok), pid()} | {error, Reason::term()}
%
start_link(Name, Mod, Args, Options) ->
  gen_server:start_link(Name, ?MODULE, {init, Mod, Args}, Options).

%%%%%
%% @spec call(server_ref(), term()) -> atom(ok)
%
call(Name, Request) -> gen_server:call(Name, Request).
		       
%%%%%
%% @spec call(server_ref(), term(), integer())  | atom(infinity)) -> atom(ok)
%
call(Name, Request, Timeout) -> gen_server:call(Name, Request, Timeout).
			 

%%%%%
%% @spec cast(server_ref(), term()) -> atom(ok)
%
cast(Name, Request) -> gen_server:cast(Name, Request).

%%%%%
%% @spec cast(client(), term()) -> atom(ok)
%
reply(From, Reply) -> gen_server:reply(From, Reply).

%%%%%
%() -> .
%
% = 
%
abcast(Name, Request) -> gen_server:abcast(Name, Request).


%%%%%
%() -> .
%
% = 
%
abcast(Nodes, Name, Request) -> gen_server:abcast(Nodes, Name, Request).
   
%%%%%
%() -> .
%
% = 
%
multi_call(Name, Req) -> gen_server:multi_call(Name, Req).


%%%%%
%() -> .
%
% = 
%
multi_call(Nodes, Name, Req) -> gen_server:multi_call(Nodes, Name, Req).
  
  
%%%%%
%() -> .
%
% = 
%
multi_call(Nodes, Name, Req, Timeout) ->
  gen_server:multi_call(Nodes, Name, Req, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED GEN_SERVER CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
%() -> .
%
% = 
%
init({init, Mod, Args}) ->
  case catch Mod:init(Args) of
    {ok, State} -> {ok, #server_state{module = Mod, state = State}};
    {ok, State, Timeout} ->
      {ok, #server_state{module = Mod, state = State}, Timeout};      
    Else -> Else
  end.    
  

%%%%%
%() -> .
%
% = 
%
handle_call(Request, From, State) ->
  Module = State#server_state.module,
  State1 = State#server_state.state,
  case catch Module:handle_call(Request, From, State1) of
    {reply, Reply, NState} ->
      {reply, Reply, State#server_state{state = NState}};
    {reply, Reply, NState, Timeout} ->
      {reply, Reply, State#server_state{state = NState}, Timeout};
    {noreply, NState} ->
      {noreply, State#server_state{state = NState}};
    {noreply, NState, Timeout} ->
      {noreply, State#server_state{state = NState}, Timeout};
    {stop, Reason, Reply, NState} ->
      {stop, Reason, Reply, State#server_state{state = NState}};
    {stop, Reason, NState} ->
      {stop, Reason, State#server_state{state = NState}};
    Other -> Other
  end.


%%%%%
%() -> .
%
% = 
%
handle_cast(Request, State) ->
  Module = State#server_state.module,
  State1 = State#server_state.state,
  case catch Module:handle_cast(Request, State1) of
    {noreply, NState} ->
      {noreply, State#server_state{state = NState}};
    {noreply, NState, Timeout} ->
      {noreply, State#server_state{state = NState}, Timeout};
    {stop, Reason, NState} ->
      {stop, Reason, State#server_state{state = NState}};
    Other -> Other
  end.

%%%%%
%() -> .
%
% = 
%
handle_info(Info, State) ->
  Module = State#server_state.module,
  State1 = State#server_state.state,
  case Info of
    {gs, Id, EventType, Data, Args} ->
      From = {gs, Id},
      case catch Module:handle_gs(Id, EventType, Data, Args, State1) of
	{reply, Reply, NState} ->
	  reply(From, Reply),
	  {noreply, State#server_state{state = NState}};
	{reply, Reply, NState, Timeout} ->
	  reply(From, Reply),
	  {noreply, State#server_state{state = NState}, Timeout};
	{noreply, NState} ->
	  {noreply, State#server_state{state = NState}};
	{noreply, NState, Timeout} ->
	  {noreply, State#server_state{state = NState}, Timeout};
	{stop, Reason, NState} ->
	  {stop, Reason, State#server_state{state = NState}};
	Other -> Other
      end;
    _ ->
      case catch Module:handle_info(Info, State1) of
	{noreply, NState} ->
	  {noreply, State#server_state{state = NState}};
	{noreply, NState, Timeout} ->
	  {noreply, State#server_state{state = NState}, Timeout};
	{stop, Reason, NState} ->
	  {stop, Reason, State#server_state{state = NState}};
	Other -> Other
      end
  end.

%%%%%
%() -> .
%
% = 
%
terminate(Reason, State) ->
  Module = State#server_state.module,
  State1 = State#server_state.state,
  Module:terminate(Reason, State1).
      
   
%%%%%
%() -> .
%
% = 
%
code_change(OldVsn, State, Extra) ->
  Module = State#server_state.module,
  State1 = State#server_state.state,
  case catch Module:code_change(OldVsn, State1, Extra) of
    {ok, NState} -> {ok, State#server_state{state = NState}};
    Else -> Else
  end.  
  
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
