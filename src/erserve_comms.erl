-module(erserve_comms).

%%%_* Exports ------------------------------------------------------------------
-export([ eval/2
        , eval_void/2
        , receive_connection_ack/1
        , set_variable/4
        ]).


%%%_* Includes -----------------------------------------------------------------
-include_lib("src/erserve.hrl").


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

set_variable(Sock, Name, Type,   Value) when is_atom(Name) ->
  set_variable(Sock, atom_to_list(Name), Type, Value);
set_variable(Sock, Name, int,    Value)                    ->
  set_variable(Sock, Name, array_int, [Value]);
set_variable(Sock, Name, double, Value)                    ->
  set_variable(Sock, Name, array_double, [Value]);
set_variable(Sock, Name, Type, Value)                      ->
  ok = send_set_variable(Sock, Name, Type, Value),
  receive_reply(Sock).


%%%_* Internal functions -------------------------------------------------------
send_eval(Sock, Expr) ->
  Message = message(eval, Expr),
  gen_tcp:send(Sock, Message).

send_eval_void(Sock, Expr) ->
  Message = message(eval_void, Expr),
  gen_tcp:send(Sock, Message).

send_set_variable(Sock, Name, Type, Value) ->
  Message = message({set_variable, Type}, {Name, Value}),
  io:format("Sending~n"),
  binpp:pprint(iolist_to_binary(Message)),
  gen_tcp:send(Sock, Message).


%%%_* Data receiving functions -------------------------------------------------
receive_reply(Sock) ->
  {ok, AckCode} = gen_tcp:recv(Sock, 4),
  case AckCode of
    <<?resp_ok>> -> receive_reply_1(Sock);
    _            -> receive_reply_error(AckCode, Sock)
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

receive_item(Sock, ?dt_sexp) ->
  {ok, Header} = gen_tcp:recv(Sock, 4),
  << SexpType:8/integer-little
   , SexpLength:24/integer-little
  >> = Header,
  Item = receive_sexp(Sock, SexpType, SexpLength),
  {Item, SexpLength + 4}.

receive_sexp(Sock, Type,             Length) when Type > ?xt_has_attr ->
  %% SEXP has attributes, so we need to read off the attribute SEXP
  %% before we get to this expression proper
  {AttrSexp, AttrSexpLength} = receive_item(Sock, ?dt_sexp),
  Sexp                       = receive_sexp(Sock,
                                            Type - ?xt_has_attr,
                                            Length - AttrSexpLength),
  {AttrSexp, Sexp};
receive_sexp(Sock, ?xt_str,          Length)                          ->
  Strings = receive_string_array(Sock, Length),
  hd(Strings);
receive_sexp(Sock, ?xt_vector,       Length)                          ->
  Vector = receive_vector(Sock, Length, []),
  Vector;
receive_sexp(Sock, ?xt_symname,      Length)                          ->
  receive_sexp(Sock, ?xt_str, Length);
receive_sexp(Sock, ?xt_list_tag,     Length)                          ->
  TagList = receive_tagged_list(Sock, Length, []),
  TagList;
receive_sexp(Sock, ?xt_lang_notag,   Length)                          ->
  receive_sexp(Sock, ?xt_vector, Length);
receive_sexp(Sock, ?xt_vector_exp,   Length)                          ->
  receive_sexp(Sock, ?xt_vector, Length);
receive_sexp(Sock, ?xt_clos,         Length)                          ->
  Closure = receive_closure(Sock, Length),
  Closure;
receive_sexp(Sock, ?xt_array_int,    Length)                          ->
  Array = receive_int_array(Sock, Length, []),
  Array;
receive_sexp(Sock, ?xt_array_double, Length)                          ->
  Array = receive_double_array(Sock, Length, []),
  Array;
receive_sexp(Sock, ?xt_array_str,    Length)                          ->
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
  %% Strip off '\01'-padding, split on null terminators, strip more padding
  String     = string:strip(binary_to_list(Data), right, 1),
  Strings0   = string:tokens(String, [0]),
  lists:map(fun(Str) ->
                string:strip(Str, left, 1)
            end, Strings0).

receive_tagged_list(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
receive_tagged_list( Sock, Length, Acc) ->
  {Value, ValueLength} = receive_item(Sock, ?dt_sexp),
  {Key,   KeyLength}   = receive_item(Sock, ?dt_sexp),
  Item                 = {Key, Value},
  NewAcc               = [Item|Acc],
  RemainingLength      = Length - KeyLength - ValueLength,
  receive_tagged_list(Sock, RemainingLength, NewAcc).

receive_vector(_Sock, 0,      Acc) ->
  lists:reverse(Acc);
receive_vector( Sock, Length, Acc) ->
  {Item, UsedLength} = receive_item(Sock, ?dt_sexp),
  NewAcc = [Item|Acc],
  RemainingLength = Length - UsedLength,
  receive_vector(Sock, RemainingLength, NewAcc).


%%%_* Data sending functions ---------------------------------------------------
message(eval,      String) ->
  Body   = dt(string, String),
  Length = iolist_size(Body),
  [ header(?cmd_eval, Length)
  , Body
  ];
message(eval_void, String) ->
  Body   = dt(string, String),
  Length = iolist_size(Body),
  [ header(?cmd_void_eval, Length)
  , Body
  ];
message({set_variable, Type}, {Name0, Value0}) ->
  Name   = dt(string, Name0),
  Value  = dt(sexp,   {Type, Value0}),
  Body   = [Name, Value],
  Length = iolist_size(Body),
  [ header(?cmd_set_sexp, Length)
  , Body
  ].

header(Command, Length) ->
  << Command:32/integer-little
   , Length:32/integer-little
   , 0:32/integer-little        % offset
   , 0:32/integer-little        % currently only support 32-bit lengths
  >>.

dt(int,    Int) ->
  [ << ?dt_int:8/integer-little
     , ?size_int:24/integer-little >>
  , transfer_int(Int)
  ];
dt(double, Double) ->
  [ << ?dt_double:8/integer-little
     , ?size_double:24/integer-little >>
  , transfer_double(Double)
  ];
dt(string, String0) ->
  String = transfer_string(String0),
  Length = iolist_size(String),
  [ << ?dt_string:8/integer-little
     , Length:24/integer-little >>
  , String
  ];
dt(sexp,   {Type, Sexp0}) ->
  Sexp   = transfer_sexp({Type, Sexp0}),
  Length = iolist_size(Sexp),
  [ << ?dt_sexp:8/integer-little
     , Length:24/integer-little >>
  , Sexp
  ].

xt(array_int, Ints)       ->
  [ xt_header(array_int, Ints)
  , lists:map(fun transfer_int/1, Ints)
  ];
xt(array_double, Doubles) ->
  [ xt_header(array_double, Doubles)
  , lists:map(fun transfer_double/1, Doubles)
  ];
xt(array_string, Strings) ->
  [ xt_header(array_string, Strings)
  , lists:map(fun transfer_string/1, Strings)
  ].

xt_header(array_int, Ints)       ->
  Length = length(Ints) * ?size_int,
  xt_header(?xt_array_int, Length);
xt_header(array_double, Doubles) ->
  Length = length(Doubles) * ?size_double,
  xt_header(?xt_array_double, Length);
xt_header(array_string, Strings) ->
  Length = lists:foldl(fun(Str, Acc) ->
                           Acc + iolist_size(transfer_string(Str))
                       end, 0, Strings),
  xt_header(?xt_array_str, Length);
xt_header(Type, Length)          ->
  << Type:8/integer-little
   , Length:24/integer-little
  >>.


%% Strings are transferred with a '\00' terminator and padded out
%% with '\01' to become of a byte length that's divisible by 4.
transfer_string(String0) ->
  String  = [String0, <<0>>],
  Length0 = iolist_size(String),
  case (Length0 rem 4) of
    0 -> String;
    N -> [String, binary:copy(<<1>>, 4 - N)]
  end.

transfer_int(Int) ->
  <<Int:(?size_int * 8)/integer-little>>.

transfer_double(Double) ->
  <<Double:(?size_double * 8)/float-little>>.

transfer_sexp({Type, Data}) ->
  xt(Type, Data).


%%%_* Error handling -----------------------------------------------------------
error_from_code(?err_auth_failed)     ->
  auth_failed;
error_from_code(?err_conn_broken)     ->
  connection_broken;
error_from_code(?err_inv_cmd)         ->
  invalid_command;
error_from_code(?err_inv_par)         ->
  invalid_parameters;
error_from_code(?err_r_error)         ->
  r_error_occurred;
error_from_code(?err_io_error)        ->
  io_error;
error_from_code(?err_not_open)        ->
  file_not_open;
error_from_code(?err_access_denied)   ->
  access_denied;
error_from_code(?err_unsupported_cmd) ->
  unsupported_command;
error_from_code(?err_unknown_cmd)     ->
  unknown_command;
error_from_code(?err_data_overflow)   ->
  data_overflow;
error_from_code(?err_object_too_big)  ->
  object_too_big;
error_from_code(?err_out_of_mem)      ->
  out_of_memory;
error_from_code(?err_ctrl_closed)     ->
  control_pipe_closed;
error_from_code(?err_session_busy)    ->
  session_busy;
error_from_code(?err_detach_failed)   ->
  unable_to_detach_session;
error_from_code(Other)                ->
  {unknown_error, Other}.
