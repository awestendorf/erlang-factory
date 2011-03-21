%%%----------------------------------------------------------------------
%%% File    : web.erl
%% @author Jan Henry Nystrom <henry.nystrom@erlang-consulting.com>
%% @doc The Web backend of ATM example for the OTP course.
%%% Created : 4 July 2006 by Jan Henry Nystrom 
%% @copyright 2006 Erlang Consulting Ltd (www.erlang-consulting.com)
%% @version '2.0'
%%%----------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(web).
-version('2.0').
-author('henry.nystrom@erlang-consulting.com').
-copyright('Copyright (c) 2006 Erlang Consulting Ltd (www.erlang-consulting.com)').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../include/backend.hrl").
-include_lib("kernel/include/inet.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
% Session Interface
%
-export([new_cookie_session/1,
	 cookieval_to_opaque/1,
	 delete_cookie_session/1
	]).

%%%%%
% Management Interface
%
-export([start/0, start_link/0, stop/0]).

%%%%%
% gen_server callbacks
%
-export([init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%%%%
% Webpage callbacks
%
-export([do/2, do/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SERVER_ADDR, {127,0,0,1}).
-define(SERVER_PORT, 8080).
-define(HTTP_SERVER, "http://127.0.0.1:8080").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @type session() = {user::string(), pin::string()}
%%                    
%
-record(state, {}).
-record(session, {id, user, pin}).
-record(cookie, {key, value}).
-record(arg, {key, value}).
-record(headers, {cookies = [],
		  args = [],
		  page = login,
		  post = false
		 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / Session Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_cookie_session(Session) ->
    gen_server:call(?MODULE, {new_cookie_session, Session}).

delete_cookie_session(SessionId) ->
    gen_server:cast(?MODULE, {delete_cookie_session, SessionId}).

cookieval_to_opaque(SessionId) ->
    gen_server:call(?MODULE, {cookieval_to_opaque, SessionId}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / Management Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_server:start({local, ?MODULE}, ?MODULE, no_args, []).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:cast(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% 
%
init(no_args) ->
    session_couter = ets:new(session_couter, [named_table]),
    sessions = ets:new(sessions, [named_table, {keypos, #session.id}]),
    ets:insert(session_couter, {counter, 0}),
    ok = ensure_started(inets),
    Root = get_root(),
    ServerConf =
	[{port, ?SERVER_PORT},
	 {server_name, "ebank"},
	 {bind_address, ?SERVER_ADDR},
	 {server_root, filename:join([Root, "priv"])},
	 {document_root, filename:join([Root, "priv/www"])},
	 {modules, [mod_alias,
		    mod_auth,
		    mod_esi,
		    mod_actions,
		    mod_get,
		    mod_head,
		    mod_log,
		    mod_trace]},
	 {error_log, "logs/error_log.txt"},
	 {security_log, "logs/security_log.txt"},
	 {transfer_log, "logs/transfer_log.txt"},
	 {directory_index, ["index.html"]},
	 {erl_script_alias, {"/ebank", [?MODULE]}}
	 ],
    inets:start(httpd, ServerConf),
    {ok, #state{}}.

handle_call({new_cookie_session, Session}, _, State) ->
    Id = ets:update_counter(session_couter, counter, {2, 1}),
    ets:insert(sessions, Session#session{id = Id}),
    erlang:send_after(300000, self(), {delete_cookie_session, Id}),
    {reply, Id, State};
handle_call({cookieval_to_opaque, SessionId}, _, State) ->
    Reply = 
	case ets:lookup(sessions, SessionId) of
	    [Session] -> {ok, Session};
	    [] -> {error, no_session}
	end,
    {reply, Reply, State};
handle_call(Call, _, State) ->
    io:format("Unexpected call in ~p:~p~n", [?MODULE, Call]),
    {noreply, State}.

handle_cast({delete_cookie_session, SessionId}, State) ->
    ets:delete(sessions, SessionId),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    io:format("Unexpected cast in ~p:~p~n", [?MODULE, Cast]),
    {noreply, State}.

handle_info({delete_cookie_session, SessionId}, State) ->
    handle_cast({delete_cookie_session, SessionId}, State);
handle_info(Info, State) ->
    io:format("Unexpected info in ~p:~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_, _) ->
    inets:stop(httpd, {?SERVER_ADDR, ?SERVER_PORT}),
    ok.

code_change(_, State, _) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS/WEBPAGE CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do(SSID, Headers) -> do(SSID, Headers, []).

do(SSID, Headers, Args) ->
    ThePage =
	case catch the_do(Headers, Args) of
	    {ok, Page} -> Page;
	    Error ->
		TheError = lists:flatten(io_lib:format("~p", [Error])),
		expand({ehtml, {body, [], TheError}}, [])
	end,
    mod_esi:deliver(SSID, ThePage).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

the_do(Headers, Args) ->
    A = analyse_args(Args, analyse_head(Headers, #headers{})),
    case top(A) of
	ok -> {ok, expand(page(A), [])};
	X -> {ok, expand(X, [])}
    end.

%%%%%
%% @spec () -> 
%
page(A = #headers{post = true, page = Page}) ->
    case collect_vars(post_vars(Page), A) of
	{ok, Values} -> post_process(Page, Values, A);
	_ -> post_fail(Page, A)
    end;
page(A = #headers{page = Page}) ->
    CSS =
	case Page of
	    login -> css_head("Erlang Bank - Login");
	    settings -> css_head("Erlang Bank - Settings");
	    transactions -> css_head("Erlang Bank - Transactions");
	    accounts -> css_head("Erlang Bank - Accounts")
	end,
    Banner = banner(),
    Logout = case Page of
		 login -> [];
		 _ ->
		     {ok, Session, _Cookie} = check_cookie(A),
		     logout_strip(Session#session.user)
	     end,
    Body = body(Page, A),
    [CSS, Banner, Logout, Body, bot()].


%%%%%
%% @spec () -> 
%% This function is calle first in all out yaws files,
%% it will autologin users that are not logged in
%
top(A = #headers{page = login}) ->
  case check_cookie(A) of
    {ok, _Sessionion, _Cookie} ->
	  redirect(none, "/ebank/web:do/page/settings");
    _ -> ok
  end;
top(A) ->
  case check_cookie(A) of
    {ok, _Sessionion, _Cookie} -> ok;
    _ -> redirect(none, "/ebank/web:do/page/login")
  end.

ensure_started(Application) ->
    case lists:keymember(Application, 1, application:which_applications()) of
	true -> ok;
	false -> application:start(inets)
    end.


get_root() ->
    {ok, CWD} = file:get_cwd(),
    Parts = lists:reverse(filename:split(CWD)),
    Parts1 =
	lists:dropwhile(fun(Dir) -> not lists:prefix("ebank", Dir) end,
			Parts),
    filename:join(lists:reverse(Parts1)).

analyse_head([], Acc) -> Acc;
analyse_head([{http_cookie,Cookies}| T], Acc=#headers{cookies = CookiesIn}) ->
    AttrValues = string:tokens(Cookies, "; "),
    CookiesTuples =
	lists:map(fun(AttrValue) ->
			  [Name, Value] = string:tokens(AttrValue, "="),
			  #cookie{key = Name, value = list_to_integer(Value)}
		  end,
		  AttrValues),
    analyse_head(T, Acc#headers{cookies = CookiesTuples ++ CookiesIn});
analyse_head([{path_info, Info} | T], Acc) ->
    [Path | _] = string:tokens(Info, "?"),
    Acc1 =
	case string:tokens(Path, "/") of
	    ["page", Page] -> Acc#headers{page = list_to_atom(Page)};
	    ["post", Page] ->
		Acc#headers{page = list_to_atom(Page), post = true}
	end,
    analyse_head(T, Acc1);
analyse_head([_ | T], Acc) -> analyse_head(T, Acc).

analyse_args(Args, Acc) ->
    case string:tokens(Args, "?") of
	[_, TheArgs] ->
	    SplitArgs = [X || X <- string:tokens(TheArgs, "&")],
	    Result =
		lists:map(fun(AnArg) ->
				  [Name, Value] = string:tokens(AnArg, "="),
				  #arg{key = Name,
				       value = lists:map(fun($+) -> $ ;
							    (Y) -> Y
							 end,
							 Value)}
			  end,
			  SplitArgs),
	    Acc#headers{args = Result};
	_ -> Acc
    end.

%%%%%
%% @spec () -> 
%
body(login, _) ->
  {ehtml,
   {form, [{method, get},
	   {action, "/ebank/web:do/post/login"}],
    {table, [{width, "42%"},
	     {border, "0"},
	     {cellspacing, "10"},
	     {cellpadding, "0"}],
     [{tr, [],
       [{td, [{width, "10%"}, {class, "body"}], "Your Name"},
	{td, [{width, "90%"}], {input, [{name, user},
					{type, text}]}}]},
      {tr, [],
       [{td, [{width, "10%"}, {class, "body"}], "Your Pin"},
	{td, [{width, "90%"}], {input, [{name, pin},
					{type, password}]}}]},
      {tr, [],
       [{td, [{width, "10%"}], {input, [{type, submit},
					{value, "Login"}]}}]}
     ]
    }
   }
  };
body(settings, _) ->
  TDS =
    fun(Var, VarName) ->
	[{td, [{width, "15%"}, {class, body}], VarName},
	 {td, [{width, "75%"}], {input, [{name, Var}, {type, password}]}}]
    end,
  {ehtml,
   [heading("/gifs/settings_head.gif"),
    {form, [{method, get},
	   {action, "/ebank/web:do/post/changepin"}],
    {table, [{width, "42%"},
	     {border, "0"},
	     {cellspacing, "0"},
	     {cellspacing, "0"}],
     [{tr, [], TDS(oldPin, "Old Pin")},
      {tr, [], TDS(newPin, "New Pin")},
      {tr, [], TDS(newPin1, "New Pin(repeated)")},
      {tr, [], [{td, [{width, "10%"}], {input, [{type, submit},
						{value, "Submit"}]}}]}
      ]}},
    headings_map()
   ]};
body(transactions, A) ->
  {ehtml,
   [heading("/gifs/transactions_head.gif"),
    transactions_accounts(A),
    headings_map()
   ]};
body(accounts, A) ->
  TRT =
    fun(Text) ->
	{tr, [], {td, [{class,body}, {colspan, "3"}], {b, [], Text}}}
    end,
  TRI =
    fun(Var, VarName) ->
	{tr, [], [{td, [{width, "15%"}, {class, body}], VarName},
		  {td, [{width, "75%"}], {input, [{name, Var},
						  {type, text}]}}]}
    end,
  {Available, Accounts} = accounts_get_accounts(A),
  {ehtml,
   [heading("/gifs/accounts_head.gif"),
    {form, [{method, get},
	   {action, "/ebank/web:do/post/transfer"}],
    {table, [{width, "42%"},
	     {border, "0"},
	     {cellspacing, "0"},
	     {cellspacing, "0"}],
     [TRT("Available accounts"),
      Accounts,
      TRT(""),
      {tr, [],
       {td, [{class,body}, {colspan, "3"}],
	"Total amount: &pound;" ++ integer_to_list(Available)}},
      TRT(""),
      TRT("Transfer money between accounts"),
      TRI(amount, "Amount"),
      TRI(from, "From account"),
      TRI(to, "To account"),
      {tr, [], [{td, [{width, "10%"}], {input, [{type, submit},
						{value, "Submit"}]}}]}
      ]}},
    headings_map()
   ]}.

%%%%%
%% @spec () -> 
%
post_vars(login) -> ["user", "pin"];
post_vars(changepin) -> ["oldPin", "newPin", "newPin1"];
post_vars(transfer) -> ["amount", "from", "to"];
post_vars(logout) -> [].

%%%%%
%% @spec () -> 
%
post_fail(login, _A) -> redirect(none, "/ebank/web:do/page/login");
post_fail(changepin, _A) -> redirect(none, "/ebank/web:do/page/settings");
post_fail(transfer, _A) -> redirect(none, "/ebank/web:do/page/accounts");
post_fail(logout, _A) -> redirect(none, "/ebank/web:do/page/settings").

%%%%%
%% @spec () -> 
%
post_process(login, [User, Pwd], A) ->
  case backend:pin_valid(User, Pwd) of
    true ->
      Session = #session{user = User, pin = Pwd},
      Cookie = ?MODULE:new_cookie_session(Session),
      redirect(Cookie, "/ebank/web:do/page/settings");
    false -> post_fail(login, A)
  end;
post_process(changepin, [Old, New, New], A) ->
  {ok, Session, _Cookie} = check_cookie(A),
  case backend:change_pin(Session#session.user, Old, New) of
    ok -> redirect(delete, "/ebank/web:do/post/logout");
    {error, _Error} -> post_fail(changepin, A)
  end;
post_process(changepin, _, A) -> post_fail(changepin, A);
post_process(transfer, [Amount, From, To], A) ->
  {ok, Session, _Cookie} = check_cookie(A),
  Amount1 = list_to_integer(Amount),
  From1 = list_to_integer(From),
  To1 = list_to_integer(To),
  case backend:transfer(Amount1, From1, To1, Session#session.pin) of
    ok -> redirect(none, "/ebank/web:do/page/accounts");
    {error, _Reason} -> post_fail(transfer, A)
  end;
post_process(logout, [], A) ->
  case check_cookie(A) of
    {ok, _Sessionion, Cookie} -> ?MODULE:delete_cookie_session(Cookie);
    {error, _Reason} -> ok
  end,
  redirect(delete, "/ebank/web:do/page/login").

%%%%%
%% @spec () -> 
%
collect_vars(L, A) -> collect_vars1(L, [], A).

%%%%%
%% @spec () -> 
%
collect_vars1([], Acc, _) -> {ok, lists:reverse(Acc)};
collect_vars1([Var | T], Acc, A) ->
  case lists:keysearch(Var, #arg.key, A#headers.args) of
    {value, #arg{value = Value}} -> collect_vars1(T, [Value | Acc], A);
    _ -> false
  end.
      

%%%%%
%% @spec () -> 
%
%% this function extracts the session from the cookie
check_cookie(A) ->
  case lists:keysearch("ssid", #cookie.key, A#headers.cookies) of
      {value, #cookie{value = Val}} ->
	  case ?MODULE:cookieval_to_opaque(Val) of
	      {ok, Session} -> {ok, Session, Val};
	      Else -> Else
	  end;
      false -> {error, nocookie}
  end.

%%%%%
%% @spec () -> 
%
%% generate a css head  the title of the page set dynamically
css_head(PageTitle) ->
    Z = 
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>

<head>
 <meta name=\"keywords\" content=\"The ETC Bank\">
 <title>",
     PageTitle,
     "</title>
 <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
 <link rel=\"stylesheet\" type=\"text/css\" href=\"/erl_bank.css\">
</head>

<body bgcolor=\"#FFFFFF\" text=\"#000000\" margin_left=\"0\" margin_top=\"0\">

"
    ],
    {html, Z}.

%%%%%
%% @spec () -> 
%
banner() ->
  Z =
    ["<img src=\"/gifs/header.gif\" width=\"600\" height=\"124\">"],
  {html, Z}.

%%%%%
%% @spec () -> 
%
transactions_accounts(A) ->
  {ok, Session, _Cookie} = check_cookie(A),
  Accounts = backend:account(Session#session.user),
  lists:foldl(fun(#account{no = No, balance = B, transactions = T}, Acc) ->
		  [{table, [{width, "250"},
			    {border, "0"},
			    {cellspacing, "0"},
			    {cellpadding, "2"}],
		    [{tr, [], [{td, [{class, "body"}, {colspan, "3"}],
				{b, [], "Account " ++ integer_to_list(No)}}]} |
		     transactions_trans(T)] ++
		    [{tr, [], [{td, [{class, "body"}, {width, "94"}],
				{b, [], "Balance "}},
			       {td, [{class, "body"}], "&nbsp;"},
			       {td, [{class, "body"}],
				{b, [], "&pound;" ++ integer_to_list(B)}}
			      ]
		     }
		    ]
		   },
		   {br, []} | Acc]
	      end,
	      [],
	      Accounts).

%%%%%
%% @spec () -> 
%
transactions_trans(T) ->
  lists:foldl(fun({Type, Date, Sum}, Acc) ->
		  [transactions_trans1(Type, Date, Sum) | Acc]
	      end,
	      [],
	      T).

%%%%%
%% @spec () -> 
%
transactions_trans1(Type, {Year, Month, Day}, Sum) ->
  Date = integer_to_list(Day) ++ "-" ++
    integer_to_list(Month) ++ "-" ++ integer_to_list(Year),
  {tr, [], [{td, [{class, "body"}, {width, "94"}], Date},
	    case Type of
	      deposit -> {td, [{class, "body"}, {width, "14"}], "&nbsp"};
	      withdraw -> {td, [{class, "body"}, {width, "14"}], "-"}
	    end,
	    {td, [{class, "body"}, {width, "142"}], integer_to_list(Sum)}]}.

		  
accounts_get_accounts(A) ->
  {ok, Session, _Cookie} = check_cookie(A),
  Accounts = backend:account(Session#session.user),
  TRA =
    fun(No, Amount) ->
	{tr, [],
	 [{td, [{class,body}], "Account "  ++ integer_to_list(No) ++ ":"},
	  {td, [{class,body}], "&pound;" ++ integer_to_list(Amount)}
	 ]}
    end,
  lists:foldl(fun(#account{balance = B, no = No}, {Total, List}) ->
		  {B + Total, [TRA(No, B) | List]}
	      end,
	      {0, []},
	      Accounts).


%%%%%
%% @spec () -> 
%
logout_strip(User) ->
  Z = {p, [], [{b, [], User},
	       " is logged in. ",
	       {a, [{href, "/ebank/web:do/post/logout"}], "Click here"},
	       " to log out."]},
  {ehtml, Z}.

%%%%%
%% @spec () -> 
%
%% kinda hackish since we us ehtml
bot() -> {html, "</body> \n </html> \n"}.

%%%%%
%% @spec () -> 
%
heading(Gif) ->
  {img, [{src, Gif},
	 {width, "600"},
	 {height, "82"},
	 {usemap, "#Map"},
	 {border, "0"}
	]}.

%%%%%
%% @spec () -> 
%
headings_map() ->
  {map, [{name, "Map"}],
   [{area, [{shape, rect},
	    {coords, "15,2,167,21"},
	    {href, "/ebank/web:do/page/transactions"}]},
    {area, [{shape, rect},
	    {coords, "202,2,354,21"},
	    {href, "/ebank/web:do/page/accounts"}]},
    {area, [{shape, rect},
	    {coords, "386,2,538,21"},
	    {href, "/ebank/web:do/page/settings"}]}
   ]}.

redirect(none, Page) ->
    [{html,
      ["Location: ", ?HTTP_SERVER, Page, "\r\n",
       "Content-type: text/html\r\n\r\n"
      ]}];
redirect(delete, Page) ->
    [{html,
      ["Set-Cookie: ssid = ", integer_to_list(0), "; Path=/ebank/web:do/; Max-Age=0\r\n",
       "Location: ", ?HTTP_SERVER, Page, "\r\n",
       "Content-type: text/html\r\n\r\n"
     ]}];
redirect(SSID, Page) ->
    [{html,
      ["Set-Cookie: ssid = ", integer_to_list(SSID), "; Path=/ebank/web:do/; Max-Age=300\r\n",
       "Location: ", ?HTTP_SERVER, Page, "\r\n",
       "Content-type: text/html\r\n\r\n"
      ]}].



expand([], Acc) -> lists:reverse(Acc);
expand([[] | T], Acc) -> expand(T, Acc);
expand([{html, H} | T], Acc) -> expand(T, [H | Acc]);
expand([{ehtml, E} | T], Acc) -> expand(T, [ehtml_expand(E) | Acc]).


%%%Nicked from YAWS


%% ------------------------------------------------------------
%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         binary() | character()
%% Tag          = atom()
%% Attrs = [{Key, Value}]  or {EventTag, {jscall, FunName, [Args]}}
%% Key          = atom()
%% Value = string()
%% Body  = EHTML

ehtml_expand(Ch) when Ch >= 0, Ch =< 255 -> Ch; %yaws_api:htmlize_char(Ch);
ehtml_expand(Bin) when binary(Bin) -> Bin; % yaws_api:htmlize(Bin);

%%!todo (low priority) - investigate whether tail-recursion would be of any 
%% benefit here instead of the current ehtml_expand(Body) recursion.
%%                - provide a tail_recursive version & add a file in the 
%% benchmarks folder to measure it.
                                                %
ehtml_expand({Tag}) -> 
    ["<", atom_to_list(Tag), " />"];
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({Tag, Attrs}) ->
    NL = ehtml_nl(Tag),
    [NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), "></",
     atom_to_list(Tag), ">"];
ehtml_expand({Tag, Attrs, Body}) when atom(Tag) ->
    Ts = atom_to_list(Tag),
    NL = ehtml_nl(Tag),
    [NL, "<", Ts, ehtml_attrs(Attrs), ">", ehtml_expand(Body), "</", Ts, ">"];
ehtml_expand([H|T]) -> [ehtml_expand(H)|ehtml_expand(T)];
ehtml_expand([]) -> [].



ehtml_attrs([]) -> [];
ehtml_attrs([Attribute|Tail]) when atom(Attribute) ->
    [[$ |atom_to_list(Attribute)]|ehtml_attrs(Tail)];
ehtml_attrs([Attribute|Tail]) when list(Attribute) ->
    [" ", Attribute|ehtml_attrs(Tail)];
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = if atom(Value) -> [$",atom_to_list(Value),$"];
                     list(Value) -> [$",Value,$"];
                     integer(Value) -> [$",integer_to_list(Value),$"];
                     float(Value) -> [$",float_to_list(Value),$"]
                  end,
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)];
ehtml_attrs([{check, Name, Value} | Tail]) ->
    ValueString = if atom(Value) -> [$",atom_to_list(Value),$"];
                     list(Value) ->
                          Q = case deepmember($", Value) of
                                  true -> $';
                                  false -> $"
                              end,
                          [Q,Value,Q];
                      integer(Value) -> [$",integer_to_list(Value),$"];
                      float(Value) -> [$",float_to_list(Value),$"]
                   end,
                   [[$ |atom_to_list(Name)], 
                    [$=|ValueString]|ehtml_attrs(Tail)].



%% Tags for which we must not add extra white space.
%% FIXME: should there be anything more in this list?

ehtml_nl(a) -> [];
ehtml_nl(br) -> [];
ehtml_nl(span) -> [];
ehtml_nl(em) -> [];
ehtml_nl(strong) -> [];
ehtml_nl(dfn) -> [];
ehtml_nl(code) -> [];
ehtml_nl(samp) -> [];
ehtml_nl(kbd) -> [];
ehtml_nl(var) -> [];
ehtml_nl(cite) -> [];
ehtml_nl(abbr) -> [];
ehtml_nl(acronym) -> [];
ehtml_nl(q) -> [];
ehtml_nl(sub) -> [];
ehtml_nl(sup) -> [];
ehtml_nl(ins) -> [];
ehtml_nl(del) -> [];
ehtml_nl(img) -> [];
ehtml_nl(tt) -> [];
ehtml_nl(i) -> [];
ehtml_nl(b) -> [];
ehtml_nl(big) -> [];
ehtml_nl(small) -> [];
ehtml_nl(strike) -> [];
ehtml_nl(s) -> [];
ehtml_nl(u) -> [];
ehtml_nl(font) -> [];
ehtml_nl(basefont) -> [];
ehtml_nl(input) -> [];
ehtml_nl(button) -> [];
ehtml_nl(object) -> [];
ehtml_nl(_) -> "\n".


deepmember(_C,[]) ->
    false;
deepmember(C,[C|_Cs]) ->
    true;
deepmember(C,[L|Cs]) when list(L) ->
    case deepmember(C,L) of
        true  -> true;
        false -> deepmember(C,Cs)
    end;
deepmember(C,[N|Cs]) when C /= N ->
    deepmember(C, Cs).


