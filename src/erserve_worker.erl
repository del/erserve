-module(erserve_worker).

-behaviour(gen_server).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ start_link/3
        ]).

%% gen_server callbacks
-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* External API -------------------------------------------------------------
-spec start_link(string(), string(), pos_integer()) ->
                    {ok, pid()} | {error, term()}.
start_link(Name, Host, Port) ->
  gen_server:start_link({local, Name}, ?MODULE, {Host, Port}, []).


%%%_* gen_server callbacks -----------------------------------------------------
init({Host, Port}) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [ {active, false}
                                           , binary
                                           , {packet, raw}
                                           ]),
  try
    ok = erserve_comms:receive_connection_ack(Sock)
  catch _:_ ->
      gen_tcp:close(Sock),
      exit(invalid_ack)
  end,
  {ok, Sock}.

handle_call({eval, Expr},                _From, Sock) ->
  Response = erserve_comms:eval(Sock, Expr),
  {reply, Response, Sock};
handle_call({eval_void, Expr},           _From, Sock) ->
  Response = erserve_comms:eval_void(Sock, Expr),
  {reply, Response, Sock};
handle_call({set_variable, Name, Value}, _From, Sock) ->
  Response = erserve_comms:set_variable(Sock, Name, Value),
  {reply, Response, Sock};
handle_call(get_pid,                     _From, Sock) ->
  {reply, self(), Sock}.


%%%_* unused gen_server callbacks ----------------------------------------------
code_change(_OldVsn, Sock, _Extra) ->
  {ok, Sock}.

handle_cast(_Msg, Sock) ->
  {noreply, Sock}.

handle_info(_Msg, Sock) ->
  {noreply, Sock}.

terminate(_Reason, _Sock) ->
  ok.
