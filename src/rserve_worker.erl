-module(rserve_worker).

-behaviour(gen_server).

%%%=============================================================================
%%% Exports
%%%=============================================================================
%%%-----------------------------------------------------------------------------
%%% Interface
%%%-----------------------------------------------------------------------------
-export([ start_link/2
        ]).

%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------
-export([ handle_call/3
        , handle_cast/2
        , init/1
        , terminate/2
        ]).


%%%=============================================================================
%%% External API
%%%=============================================================================
start_link(Host, Port) ->
  gen_server:start_link(?MODULE, {Host, Port}, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
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

handle_call(test, _From, Sock) ->
  {ok, Response} = rserve_comms:eval_R(Sock, "R.version"),
  io:format("Received response~n~p~n", [Response]),
  {reply, Response, Sock}.

handle_cast(_Msg, Sock) ->
  {noreply, Sock}.

terminate(_Reason, _Sock) ->
  ok.


%%% Local variables:
%%% erlang-indent-level: 2
%%% End:
