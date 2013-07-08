%% @author Brilliant
%% @doc @todo Add description to dia_client.


-module(dia_client).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4,
		 start1/1, start2/1, 
		 stop/1, create_acr_process/4]).

start1(CallRate) ->
	start_link(CallRate, "127.0.0.1", 3868, 1).
start2(CallRate) ->
	start_link(CallRate, "127.0.0.1", 3868, 2).
start_link(CallRate, Addr, Port, RT)->
	gen_server:start_link(?MODULE, [CallRate, Addr, Port, RT], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {timer = undefined, callrate = 0, addr, port, rt}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([CallRate, Addr, Port, RT]) ->
	diameter:start(),
	client:start(),
	{ok, TRef} = timer:send_interval(1000, start_to_send),
    {ok, #state{callrate=CallRate, addr=Addr, port=Port, rt=RT, timer = TRef}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(stop, _From, State)->
	{stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(start_to_send, #state{addr = Addr, port = Port, rt = RT, callrate = CallRate} = State)->
	spawn(?MODULE, create_acr_process, [Addr, Port, RT, CallRate]),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{timer = TRef} = _State) ->
	timer:cancel(TRef),
	client:stop(),
	diameter:stop(),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_acr_process(Addr, Port, RT, CallRate)->
	T1 = erlang:now(),
	client:connect({tcp, Addr, Port}),
	send_acr(RT, 1, CallRate),
	io:format("spend ~p msec ~n", [timer:now_diff(erlang:now(), T1)/1000]),
	ok.

send_acr(_RT, Init, End) when Init > End ->
	ok;
send_acr(1, Init, End)->
	client:call(1, Init),
	send_acr(1, Init+1, End);
send_acr(2, Init, End) ->
	SId = client:call(2, Init),
	client:call(SId, 3, Init+1),
	client:call(SId, 4, Init+2),
	send_acr(2, Init+3, End).
