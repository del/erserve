-module(erserve_comms).

%%%_* Exports ------------------------------------------------------------------
-export([ eval/2
        , eval_void/2
        , receive_connection_ack/1
        , set_variable/3
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(HEADER_LENGTH,        16).
-define(DT_HEADER_LENGTH,     4).

-define(R_CMD_VOID_EVAL,      2).
-define(R_CMD_EVAL,           3).
-define(R_CMD_SET_SEXP,       32).
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

-define(XT_INT,               1).
-define(XT_DOUBLE,            2).
-define(XT_STR,               3).
-define(XT_LANG,              4).
-define(XT_VECTOR,            16).
-define(XT_LIST,              17).
-define(XT_CLOS,              18).
-define(XT_SYMNAME,           19).
-define(XT_LIST_TAG,          21).
-define(XT_LANG_NOTAG,        22).
-define(XT_VECTOR_EXP,        26).
-define(XT_ARRAY_INT,         32).
-define(XT_ARRAY_DOUBLE,      33).
-define(XT_ARRAY_STR,         34).

-define(XT_HAS_ATTR,          128).

-define(ERR_AUTH_FAILED,      65).
-define(ERR_CONN_BROKEN,      66).
-define(ERR_INV_CMD,          67).
-define(ERR_INV_PAR,          68).
-define(ERR_R_ERROR,          69).
-define(ERR_IO_ERROR,         70).
-define(ERR_NOT_OPEN,         71).
-define(ERR_ACCESS_DENIED,    72).
-define(ERR_UNSUPPORTED_CMD,  73).
-define(ERR_UNKNOWN_CMD,      74).
-define(ERR_DATA_OVERFLOW,    75).
-define(ERR_OBJECT_TOO_BIG,   76).
-define(ERR_OUT_OF_MEM,       77).
-define(ERR_CTRL_CLOSED,      78).
-define(ERR_SESSION_BUSY,     80).
-define(ERR_DETACH_FAILED,    81).


%%%_* External API -------------------------------------------------------------
receive_connection_ack(Sock) ->
  {ok, Msg} = gen_tcp:recv(Sock, 32),
  <<"Rsrv", _Version:32, _Protocol:32, _Extra/binary>> = Msg,
  ok.

eval(Sock, Expr) ->
  ok = send_eval(Sock, Expr),
  receive_reply(Sock).

eval_void(Sock, Expr) ->
  ok = send_eval_void(Sock, Expr),
  case receive_reply(Sock) of
    {ok, []}               -> ok;
    {error, AckCode, Rest} -> {error, AckCode, Rest}
  end.

set_variable(Sock, Name, Value) when is_atom(Name) ->
  set_variable(Sock, atom_to_list(Name), Value);
set_variable(Sock, Name, Value)                    ->
  ok = send_sexp(Sock, Name, Value),
  receive_reply(Sock).


%%%_* Internal functions -------------------------------------------------------
send_eval(Sock, Expr) ->
  send_expression(Sock, Expr, ?R_CMD_EVAL).

send_eval_void(Sock, Expr) ->
  send_expression(Sock, Expr, ?R_CMD_VOID_EVAL).


%%%_* Data receiving functions -------------------------------------------------
receive_reply(Sock) ->
  {ok, AckCode} = gen_tcp:recv(Sock, 4),
  case AckCode of
    <<?R_RESP_OK>> -> receive_reply_1(Sock);
    _              -> receive_reply_error(AckCode, Sock)
  end.

receive_reply_1(Sock) ->
  {ok, Msg} = gen_tcp:recv(Sock, 12),
  << Len0:32/integer-little
   , _Offset:32/integer-little
   , Len1:32/integer-little
  >>  = Msg,
  Len = Len0 + (Len1 bsl 31),
  {ok, receive_data(Sock, Len)}.

receive_reply_error(AckCode, Sock) ->
  <<2,0,1,ErrCode>> = AckCode,
  Error             = error_from_code(ErrCode),
  {ok, Rest}        = gen_tcp:recv(Sock, 0),
  {error, Error, Rest}.

receive_data(Sock, Length) ->
  case lists:reverse(receive_data(Sock, Length, [])) of
    [Item] -> Item;
    List   -> List
  end.

receive_data(_Sock, 0, Acc) ->
  Acc;
receive_data( Sock, Length, Acc) ->
  {ok, Header} = gen_tcp:recv(Sock, 4),
  << Type:8/integer-little
   , ItemLength:24/integer-little
  >> = Header,
  {Item, _L} = receive_item(Sock, Type),
  NewAcc = [Item|Acc],
  RemainingLength = Length - ItemLength - 4,
  receive_data(Sock, RemainingLength, NewAcc).

receive_item(Sock, ?DT_SEXP) ->
  {ok, Header} = gen_tcp:recv(Sock, 4),
  << SexpType:8/integer-little
   , SexpLength:24/integer-little
  >> = Header,
  Item = receive_sexp(Sock, SexpType, SexpLength),
  {Item, SexpLength + 4}.

receive_sexp(Sock, Type,             Length) when Type > ?XT_HAS_ATTR ->
  %% SEXP has attributes, so we need to read off the attribute SEXP
  %% before we get to this expression proper
  {AttrSexp, AttrSexpLength} = receive_item(Sock, ?DT_SEXP),
  Sexp                       = receive_sexp(Sock,
                                            Type - ?XT_HAS_ATTR,
                                            Length - AttrSexpLength),
  {AttrSexp, Sexp};
receive_sexp(Sock, ?XT_STR,          Length)                          ->
  Strings = receive_string_array(Sock, Length),
  hd(Strings);
receive_sexp(Sock, ?XT_VECTOR,       Length)                          ->
  Vector = receive_vector(Sock, Length, []),
  Vector;
receive_sexp(Sock, ?XT_SYMNAME,      Length)                          ->
  receive_sexp(Sock, ?XT_STR, Length);
receive_sexp(Sock, ?XT_LIST_TAG,     Length)                          ->
  TagList = receive_tagged_list(Sock, Length, []),
  TagList;
receive_sexp(Sock, ?XT_LANG_NOTAG,   Length)                          ->
  receive_sexp(Sock, ?XT_VECTOR, Length);
receive_sexp(Sock, ?XT_VECTOR_EXP,   Length)                          ->
  receive_sexp(Sock, ?XT_VECTOR, Length);
receive_sexp(Sock, ?XT_CLOS,         Length)                          ->
  Closure = receive_closure(Sock, Length),
  Closure;
receive_sexp(Sock, ?XT_ARRAY_INT,    Length)                          ->
  Array = receive_int_array(Sock, Length, []),
  Array;
receive_sexp(Sock, ?XT_ARRAY_DOUBLE, Length)                          ->
  Array = receive_double_array(Sock, Length, []),
  Array;
receive_sexp(Sock, ?XT_ARRAY_STR,    Length)                          ->
  Array = receive_string_array(Sock, Length),
  Array.

receive_closure(Sock, Length) ->
  {ok, Closure} = gen_tcp:recv(Sock, Length),
  Closure.

receive_int_array(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
receive_int_array( Sock, Length, Acc) ->
  Int             = receive_int(Sock),
  NewAcc          = [Int|Acc],
  RemainingLength = Length - 4,
  receive_int_array(Sock, RemainingLength, NewAcc).

receive_int(Sock) ->
  {ok, Data}                = gen_tcp:recv(Sock, 4),
  <<Int:32/integer-little>> = Data,
  Int.

receive_double_array(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
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

receive_tagged_list(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
receive_tagged_list( Sock, Length, Acc) ->
  {Value, ValueLength} = receive_item(Sock, ?DT_SEXP),
  {Key,   KeyLength}   = receive_item(Sock, ?DT_SEXP),
  Item                 = {Key, Value},
  NewAcc               = [Item|Acc],
  RemainingLength      = Length - KeyLength - ValueLength,
  receive_tagged_list(Sock, RemainingLength, NewAcc).

receive_vector(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
receive_vector( Sock, Length, Acc) ->
  {Item, UsedLength} = receive_item(Sock, ?DT_SEXP),
  NewAcc = [Item|Acc],
  RemainingLength = Length - UsedLength,
  receive_vector(Sock, RemainingLength, NewAcc).


%%%_* Data sending functions ---------------------------------------------------
send_expression(Sock, Expr, Command) ->
  Length  = length(Expr),
  Message = << Command:32/integer-little
             , ?R_MSG_LENGTH(Length)
             , ?R_OFFSET
             , ?R_LENGTH2
             , ?DT_STRING:8/integer-little
             , ?R_EXP_LENGTH(Length)
             , ?R_EXP(Expr)
             , ?R_TERMINATE
            >>,
  gen_tcp:send(Sock, Message).

send_sexp(Sock, Name, Data) ->
  case data_type(Data) of
    int    ->
      send_int_sexp(Sock, Name, Data);
    double ->
      send_double_sexp(Sock, Name, Data)
  end.

send_int_sexp(Sock, Name0, Int) when is_integer(Int) ->
  {NameLength, Name}   = terminate_and_pad_string(Name0),
  IntLength            = 4,
  MsgLength0           = NameLength + IntLength + ?DT_HEADER_LENGTH * 3,
  {MsgLength, Padding} = length_and_padding(MsgLength0),
  Message = << ?R_CMD_SET_SEXP:32/integer-little
             , MsgLength:32/integer-little
             , ?R_OFFSET
             , ?R_LENGTH2
             , ?DT_STRING:8/integer-little
             , NameLength:24/integer-little
             , Name/binary
             , ?DT_SEXP:8/integer-little
             , (IntLength + ?DT_HEADER_LENGTH):24/integer-little
             , ?XT_ARRAY_INT:8/integer-little
             , IntLength:24/integer-little
             , Int:32/integer-little
             , Padding/binary
            >>,
  io:format("Sending~n"),
  binpp:pprint(Message),
  gen_tcp:send(Sock, Message).

send_double_sexp(_Sock, _Name, Data) when is_float(Data) ->
  ok.

data_type(Data) when is_integer(Data) ->
  int;
data_type(Data) when is_float(Data)   ->
  double.

terminate_and_pad_string(Str0) ->
  Str                = Str0 ++ [0],
  Length0            = length(Str),
  {Length, Padding} = length_and_padding(Length0),
  {Length, iolist_to_binary([Str, Padding])}.

%% Messages must be of length divisible by 4 and padded out with '\01'
length_and_padding(MsgLength0) ->
  io:format("length ~p~n", [MsgLength0]),
  PaddingLength = (4 - (MsgLength0 rem 4)),
  case PaddingLength of
    4  ->
      {MsgLength0, <<>>};
    _N ->
      MsgLength     = MsgLength0 + PaddingLength,
      Padding       = padding(PaddingLength),
      {MsgLength, Padding}
  end.

padding(Length) ->
  binary:copy(<<1>>, Length).


%%%_* Error handling -----------------------------------------------------------
error_from_code(?ERR_AUTH_FAILED)     ->
  auth_failed;
error_from_code(?ERR_CONN_BROKEN)     ->
  connection_broken;
error_from_code(?ERR_INV_CMD)         ->
  invalid_command;
error_from_code(?ERR_INV_PAR)         ->
  invalid_parameters;
error_from_code(?ERR_R_ERROR)         ->
  r_error_occurred;
error_from_code(?ERR_IO_ERROR)        ->
  io_error;
error_from_code(?ERR_NOT_OPEN)        ->
  file_not_open;
error_from_code(?ERR_ACCESS_DENIED)   ->
  access_denied;
error_from_code(?ERR_UNSUPPORTED_CMD) ->
  unsupported_command;
error_from_code(?ERR_UNKNOWN_CMD)     ->
  unknown_command;
error_from_code(?ERR_DATA_OVERFLOW)   ->
  data_overflow;
error_from_code(?ERR_OBJECT_TOO_BIG)  ->
  object_too_big;
error_from_code(?ERR_OUT_OF_MEM)      ->
  out_of_memory;
error_from_code(?ERR_CTRL_CLOSED)     ->
  control_pipe_closed;
error_from_code(?ERR_SESSION_BUSY)    ->
  session_busy;
error_from_code(?ERR_DETACH_FAILED)   ->
  unable_to_detach_session;
error_from_code(Other)                ->
  {unknown_error, Other}.
