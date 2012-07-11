-module(rserve_worker).

-behaviour(gen_server).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ start_link/1
        ]).

%% gen_server callbacks
-export([ handle_call/3
        , handle_cast/2
        , init/1
        , terminate/2
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* External API -------------------------------------------------------------
-spec start_link([ term() ]) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
  {host, Host} = lists:keyfind(host, 1, Args),
  {port, Port} = lists:keyfind(port, 1, Args),
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Host, Port}, []).


%%%_* gen_server callbacks -----------------------------------------------------
init({Host, Port}) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [ {active, false}
                                           , binary
                                           , {packet, raw}
                                           ]),
  try
    ok = rserve_comms:receive_connection_ack(Sock)
  catch _:_ ->
      gen_tcp:close(Sock),
      exit(invalid_ack)
  end,
  {ok, Sock}.

handle_call({eval, Expr}, _From, Sock) ->
  Response = rserve_comms:eval(Sock, Expr),
  {reply, Response, Sock}.

%%%_* unused gen_server callbacks ----------------------------------------------
code_change(_OldVsn, Sock, _Extra) ->
  {ok, Sock}.

handle_cast(_Msg, Sock) ->
  {noreply, Sock}.

handle_info(_Msg, Sock) ->
  {noreply, Sock}.

terminate(_Reason, _Sock) ->
  ok.


%%% Local variables:
%%% erlang-indent-level: 2
%%% End:
