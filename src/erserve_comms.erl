-module(erserve_comms).

%%%_* Exports ------------------------------------------------------------------
-export([ eval/2
        , receive_connection_ack/1
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(R_CMD_EVAL,           3:32/integer-little).
-define(R_MSG_LENGTH(Length), (Length+5):32/integer-little).
-define(R_EXP_LENGTH(Length), (Length+1):24/integer-little).
-define(R_OFFSET,             0:32/integer-little).
-define(R_LENGTH2,            0:32/integer-little).
-define(R_EXP(Exp),           (list_to_binary(Exp))/binary).
-define(R_TERMINATE,          0).

-define(R_RESP_OK,            16#10001:32/integer-little).
-define(R_RESP_ERROR,         16#10002:32/integer-little).

-define(DT_INT,               1).
-define(DT_CHAR,              2).
-define(DT_DOUBLE,            3).
-define(DT_STRING,            4).
-define(DT_BYTESTREAM,        5).
-define(DT_SEXP,              10).
-define(DT_ARRAY,             11).
-define(DT_LARGE,             64).

-define(R_DT_STRING,          ?DT_STRING:8/integer-little).

-define(XT_LANG,              4).
-define(XT_VECTOR_EXP,        26).
-define(XT_ARRAY_DOUBLE,      33).
-define(XT_ARRAY_STR,         34).


%%%_* External API -------------------------------------------------------------
receive_connection_ack(Sock) ->
  {ok, Msg} = gen_tcp:recv(Sock, 32),
  <<"Rsrv", _Version:32, _Protocol:32, _Extra/binary>> = Msg,
  ok.

eval(Sock, Command) ->
  ok = send_expression(Sock, Command),
  receive_reply(Sock).


%%%_* Internal functions -------------------------------------------------------
send_expression(Sock, Expr) ->
  Length  = length(Expr),
  Message = << ?R_CMD_EVAL
             , ?R_MSG_LENGTH(Length)
             , ?R_OFFSET
             , ?R_LENGTH2
             , ?R_DT_STRING
             , ?R_EXP_LENGTH(Length)
             , ?R_EXP(Expr)
             , ?R_TERMINATE
            >>,
  gen_tcp:send(Sock, Message).

receive_reply(Sock) ->
  {ok, AckCode} = gen_tcp:recv(Sock, 4),
  case AckCode of
    <<?R_RESP_OK>>    -> receive_reply_1(Sock);
    <<?R_RESP_ERROR>> -> receive_reply_error(Sock)
  end.

receive_reply_1(Sock) ->
  {ok, Msg} = gen_tcp:recv(Sock, 12),
  << Len0:32/integer-little
   , _Offset:32/integer-little
   , Len1:32/integer-little
  >>   = Msg,
  Len  = Len0 + (Len1 bsl 31),
  {ok, receive_data(Sock, Len)}.

receive_reply_error(Sock) ->
  gen_tcp:recv(Sock, 0),
  error.

%%%_* Data receiving functions -------------------------------------------------
receive_data(Sock, Length) ->
  lists:reverse(receive_data(Sock, Length, [])).

receive_data(_Sock, 0, Acc) ->
  Acc;
receive_data( Sock, Length, Acc) ->
  {ok, Header} = gen_tcp:recv(Sock, 4),
  << Type:8/integer-little
   , ItemLength:24/integer-little
  >> = Header,
  NewAcc = [receive_item(Sock, Type, ItemLength)|Acc],
  RemainingLength = Length - ItemLength - 4,
  receive_data(Sock, RemainingLength, NewAcc).

receive_item(Sock, ?DT_SEXP, _Length) ->
  {ok, Header} = gen_tcp:recv(Sock, 4),
  << SexpType:8/integer-little
   , SexpLength:24/integer-little
  >> = Header,
  {sexp, receive_sexp(Sock, SexpType, SexpLength)}.

receive_sexp(Sock, ?XT_ARRAY_DOUBLE, Length) ->
  Array = receive_double_array(Sock, Length, []),
  {{array, double}, Array};
receive_sexp(Sock, ?XT_ARRAY_STR,    Length) ->
  Array = receive_string_array(Sock, Length),
  {{array, string}, Array}.

receive_double_array(_Sock, 0,      Acc) ->
  Acc;
receive_double_array( Sock, Length, Acc) ->
  Double          = receive_double(Sock),
  NewAcc          = [Double|Acc],
  RemainingLength = Length - 8,
  receive_double_array(Sock, RemainingLength, NewAcc).

receive_double(Sock) ->
  {ok, Data}                 = gen_tcp:recv(Sock, 8),
  <<Double:64/float-little>> = Data,
  Double.

receive_string_array(Sock, Length) ->
  {ok, Data} = gen_tcp:recv(Sock, Length),
  %% Strip off '\01'-padding, and split on null terminators
  String     = string:strip(binary_to_list(Data), right, 1),
  string:tokens(String, [0]).
