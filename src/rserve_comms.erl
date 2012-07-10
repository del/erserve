-module(rserve_comms).

%%%=============================================================================
%%% Exports
%%%=============================================================================
-export([ eval_R/2
        , receive_connection_ack/1
        ]).


%%%=============================================================================
%%% Local definitions
%%%=============================================================================
-define(R_CMD_EVAL,           3:32/integer-little).
-define(R_MSG_LENGTH(Length), (Length+5):32/integer-little).
-define(R_EXP_LENGTH(Length), (Length+1):32/integer-little).
-define(R_OFFSET,             0:32/integer-little).
-define(R_LENGTH2,            0:32/integer-little).
-define(R_DT_STRING,          4:8/integer-little).
-define(R_EXP(Exp),           (list_to_binary(Exp))/binary).
-define(R_TERMINATE,          0).

-define(R_EXP_PREFIX,         "paste(capture.output(eval(quote({").
-define(R_EXP_SUFFIX,         "}))),collapse='\n')").

%%%=============================================================================
%%% External API
%%%=============================================================================
receive_connection_ack(Sock) ->
  {ok, Msg} = gen_tcp:recv(Sock, 32),
  io:format("Msg: ~p~n", [binary_to_list(Msg)]),
  <<"Rsrv", _Version:32, _Protocol:32, _Extra/binary>> = Msg,
  ok.

eval_R(Sock, Command) ->
  ok       = send_expression(Sock, Command),
  Response = receive_reply(Sock),
  {ok, Response}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================
send_expression(Sock, Expr0) ->
  Expr    = ?R_EXP_PREFIX ++ Expr0 ++ ?R_EXP_SUFFIX,
  Length  = length(Expr),
  io:format("Expr ~p~n", [Expr]),
  Message = << ?R_CMD_EVAL
             , ?R_MSG_LENGTH(Length)
             , ?R_OFFSET
             , ?R_LENGTH2
             , ?R_DT_STRING
             , ?R_EXP_LENGTH(Length)
             , ?R_EXP(Expr)
             , ?R_TERMINATE
            >>,
  io:format("Sending ~p~n", [Message]),
  io:format("Length ~p ~p~n", [string:len(Expr), Length]),
  gen_tcp:send(Sock, Message).

receive_reply(Sock) ->
  io:format("Receive ~p~n", [gen_tcp:recv(Sock, 0)]),
  io:format("Receive ~p~n", [gen_tcp:recv(Sock, 0)]),
  A=1,
  A=2,
  {ok, <<1, 0, 1, 0, A0/binary>>}               = gen_tcp:recv(Sock, 16),
  io:format("1 ~p~n", [A0]),
  %{ok, <<_:32, 34, Length:24/integer-little>>} = gen_tcp:recv(Sock, 8),
  io:format("Then ~p~n", [gen_tcp:recv(Sock, 8)]),
  io:format("and Then ~p~n", [gen_tcp:recv(Sock, 0)]),
  {ok, <<_:32, A:8, Length:24/integer-little>>} = gen_tcp:recv(Sock, 8),
  io:format("2 ~p len ~p~n", [A, Length]),
  {ok, BitString}                              = gen_tcp:recv(Sock, Length),
  io:format("3 ~p~n", [BitString]),
  string_decode(BitString).

string_decode(BitString) ->
  StringWithTerminator = binary:bin_to_list(BitString),
  io:format("StrWT ~p~n", [StringWithTerminator]),
  lists:sublist(StringWithTerminator, length(StringWithTerminator) - 1).


%%% Local variables:
%%% erlang-indent-level: 2
%%% End:
