-module(erserve_comms).

%%%_* Exports ------------------------------------------------------------------
-export([ receive_connection_ack/1
        , receive_reply/1
        , send_message/3
        ]).


%%%_* Includes -----------------------------------------------------------------
-include_lib("src/erserve.hrl").


%%%_* External API -------------------------------------------------------------
-spec receive_connection_ack(erserve:connection()) -> ok.
receive_connection_ack(Conn) ->
  {ok, Msg} = gen_tcp:recv(Conn, 32),
  <<"Rsrv", _Version:32, _Protocol:32, _Extra/binary>> = Msg,
  ok.

-spec receive_reply(erserve:connection()) ->
                       {ok, erserve:r_data()} |
                       {error, erserve:error_code(), binary()}.
receive_reply(Conn) ->
  {ok, AckCode} = gen_tcp:recv(Conn, 4),
  case AckCode of
    <<?resp_ok:32/integer-little>> -> receive_reply_1(Conn);
    _                              -> receive_reply_error(Conn, AckCode)
  end.

-spec send_message( erserve:connection()
                  , erserve:type()
                  , erserve:r_data()) -> ok | {error, term()}.
send_message(Conn, Type, Data) ->
  Message = message(Type, Data),
  gen_tcp:send(Conn, Message).


%%%_* Data receiving functions -------------------------------------------------
receive_reply_1(Conn) ->
  {ok, Msg} = gen_tcp:recv(Conn, 12),
  << Len0:32/integer-little
   , _Offset:32/integer-little
   , Len1:32/integer-little
  >>  = Msg,
  Len = Len0 + (Len1 bsl 31),
  {ok, receive_data(Conn, Len)}.

receive_reply_error(Conn, AckCode) ->
  <<2, 0, 1, ErrCode>> = AckCode,
  Error                = error_from_code(ErrCode),
  {ok, Rest}           = gen_tcp:recv(Conn, 0),
  {error, Error, Rest}.

receive_data(Conn, Length) ->
  case lists:reverse(receive_data(Conn, Length, [])) of
    [Item] -> Item;
    List   -> List
  end.

receive_data(_Conn, 0, Acc) ->
  Acc;
receive_data( Conn, Length, Acc) ->
  {ok, Header} = gen_tcp:recv(Conn, 4),
  << Type:8/integer-little
   , ItemLength:24/integer-little
  >> = Header,
  {Item, _L} = receive_item(Conn, Type),
  NewAcc = [Item|Acc],
  RemainingLength = Length - ItemLength - 4,
  receive_data(Conn, RemainingLength, NewAcc).

receive_item(Conn, ?dt_sexp) ->
  {ok, Header} = gen_tcp:recv(Conn, 4),
  << SexpType:8/integer-little
   , SexpLength:24/integer-little
  >> = Header,
  Item = receive_sexp(Conn, SexpType, SexpLength),
  {Item, SexpLength + 4}.

receive_sexp( Conn, Type,             Length) when Type > ?xt_has_attr ->
  %% SEXP has attributes, so we need to read off the attribute SEXP
  %% before we get to this expression proper
  {AttrSexp, AttrSexpLength} = receive_item(Conn, ?dt_sexp),
  Sexp                       = receive_sexp(Conn,
                                            Type - ?xt_has_attr,
                                            Length - AttrSexpLength),
  {AttrSexp, Sexp};
receive_sexp( Conn, Type,             Length) when Type > ?xt_large    ->
  %% SEXP is large, which means the length is coded as a
  %% 56-bit integer, enlarging the header by 4 bytes
  {ok, RestLength} = gen_tcp:recv(Conn, 4),
  FullLength = Length + (RestLength bsl 23),
  receive_sexp(Conn, Type - ?xt_large, FullLength);
receive_sexp(_Conn, ?xt_null,             0)                           ->
  null;
receive_sexp( Conn, ?xt_str,          Length)                          ->
  hd(receive_string_array(Conn, Length));
receive_sexp( Conn, ?xt_vector,       Length)                          ->
  receive_vector(Conn, Length, []);
receive_sexp( Conn, ?xt_symname,      Length)                          ->
  receive_sexp(Conn, ?xt_str, Length);
receive_sexp( Conn, ?xt_list_notag,   Length)                          ->
  receive_sexp(Conn, ?xt_vector, Length);
receive_sexp( Conn, ?xt_list_tag,     Length)                          ->
  receive_tagged_list(Conn, Length, []);
receive_sexp( Conn, ?xt_lang_notag,   Length)                          ->
  receive_sexp(Conn, ?xt_list_notag, Length);
receive_sexp( Conn, ?xt_lang_tag,     Length)                          ->
  receive_sexp(Conn, ?xt_list_tag, Length);
receive_sexp( Conn, ?xt_vector_exp,   Length)                          ->
  receive_sexp(Conn, ?xt_vector, Length);
receive_sexp( Conn, ?xt_clos,         Length)                          ->
  receive_closure(Conn, Length);
receive_sexp( Conn, ?xt_array_int,    Length)                          ->
  receive_int_array(Conn, Length, []);
receive_sexp( Conn, ?xt_array_double, Length)                          ->
  receive_double_array(Conn, Length, []);
receive_sexp( Conn, ?xt_array_str,    Length)                          ->
  receive_string_array(Conn, Length);
receive_sexp( Conn, ?xt_array_bool,   Length)                          ->
  receive_bool_array(Conn, Length);
receive_sexp( Conn, UnimplType,       Length)                          ->
  receive_unimplemented_type(Conn, UnimplType, Length).


receive_closure(Conn, Length) ->
  {ok, Closure} = gen_tcp:recv(Conn, Length),
  Closure.

receive_int_array(_Conn, 0,      Acc) ->
  lists:reverse(Acc);
receive_int_array( Conn, Length, Acc) ->
  Int             = receive_int(Conn),
  NewAcc          = [Int|Acc],
  RemainingLength = Length - 4,
  receive_int_array(Conn, RemainingLength, NewAcc).

receive_int(Conn) ->
  {ok, Data}                = gen_tcp:recv(Conn, 4),
  <<Int:32/integer-little>> = Data,
  Int.

receive_double_array(_Conn, 0,      Acc) ->
  lists:reverse(Acc);
receive_double_array( Conn, Length, Acc) ->
  Double          = receive_double(Conn),
  NewAcc          = [Double|Acc],
  RemainingLength = Length - 8,
  receive_double_array(Conn, RemainingLength, NewAcc).

receive_double(Conn) ->
  {ok, Data}                 = gen_tcp:recv(Conn, 8),
  <<Double:64/float-little>> = Data,
  Double.

receive_string_array(Conn, Length) ->
  {ok, Data} = gen_tcp:recv(Conn, Length),
  %% Strip off '\01'-padding, split on null terminators, strip more padding
  String     = string:strip(binary_to_list(Data), right, 1),
  Strings0   = string:tokens(String, [0]),
  lists:map(fun(Str) ->
                string:strip(Str, left, 1)
            end, Strings0).

receive_bool_array(Conn, Length) ->
  {ok, Data0} = gen_tcp:recv(Conn, Length),
  << N:32/integer-little
   , Data/binary
  >> = Data0,
  NBoolBits = N * ?size_bool * 8,
  << Bools:NBoolBits/bitstring
   , _Padding/binary
  >> = Data,
  lists:map(fun(1) ->
                true;
               (0) ->
                false
            end, binary_to_list(Bools)).

receive_tagged_list(_Conn, 0,      Acc) ->
  lists:reverse(Acc);
receive_tagged_list( Conn, Length, Acc) ->
  {Value, ValueLength} = receive_item(Conn, ?dt_sexp),
  {Key,   KeyLength}   = receive_item(Conn, ?dt_sexp),
  Item                 = {Key, Value},
  NewAcc               = [Item|Acc],
  RemainingLength      = Length - KeyLength - ValueLength,
  receive_tagged_list(Conn, RemainingLength, NewAcc).

receive_vector(_Conn, 0,      Acc) ->
  lists:reverse(Acc);
receive_vector( Conn, Length, Acc) ->
  {Item, UsedLength} = receive_item(Conn, ?dt_sexp),
  NewAcc = [Item|Acc],
  RemainingLength = Length - UsedLength,
  receive_vector(Conn, RemainingLength, NewAcc).

receive_unimplemented_type(Conn, Type, Length) ->
  {unimplemented_type, Type, gen_tcp:recv(Conn, Length)}.


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

xt(array_int,    Ints)       ->
  Payload = lists:map(fun transfer_int/1, Ints),
  [ xt_header(?xt_array_int, Payload)
  , Payload
  ];
xt(array_double, Doubles)    ->
  Payload = lists:map(fun transfer_double/1, Doubles),
  [ xt_header(?xt_array_double, Payload)
  , Payload
  ];
xt(array_string, Strings)    ->
  Payload = lists:map(fun transfer_string/1, Strings),
  [ xt_header(?xt_array_str, Payload)
  , Payload
  ];
xt(list_tag,     TaggedList) ->
  Payload = lists:map(fun transfer_tagged/1, TaggedList),
  [ xt_header(?xt_list_tag, Payload)
  , Payload
  ];
xt(symname,      Symbol)     ->
  Payload = transfer_string(Symbol),
  [ xt_header(?xt_symname, Payload)
  , Payload
  ];
xt(vector,       Elements)   ->
  Payload = lists:map(fun transfer_sexp/1, Elements),
  [ xt_header(?xt_vector, Payload)
  , Payload
  ];
xt(data_frame,   DataFrame)  ->
  transfer_df(DataFrame).

xt_header(Type, Payload) ->
  Length = iolist_size(Payload),
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

transfer_tagged({Tag, Value}) ->
  [ transfer_sexp(Value)
  , transfer_sexp(Tag)
  ].

transfer_df(DataFrame) ->
  Names    = df_names(DataFrame),
  RowNames = df_row_names(DataFrame),
  Values   = df_values(DataFrame),
  AttrSexp = {list_tag, [ { {symname,      "names"}
                          , {array_string, Names}
                          }
                        , { {symname,      "row.names"}
                          , {array_int,    RowNames}
                          }
                        , { {symname,      "class"}
                          , {array_string, ["data.frame"]}
                          }
                        ]},
  transfer_sexp_with_attr(AttrSexp, {vector, Values}).

transfer_sexp({Type, Data}) ->
  xt(Type, Data).

transfer_sexp_with_attr(AttrSexp, Sexp) ->
  AttrBin           = transfer_sexp(AttrSexp),
  [Header, Payload] = transfer_sexp(Sexp),
  FullPayload       = [AttrBin, Payload],
  << Type:8/integer-little
   , _Length:24/integer-little
  >> = Header,
  [ xt_header(Type + ?xt_has_attr, FullPayload)
  , FullPayload
  ].


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


%%%_* Helpers for sending lists and data frames --------------------------------
df_names(DataFrame) ->
  lists:map(fun({Name, _Type, _Values}) ->
                case Name of
                  NAtom when is_atom(NAtom) -> atom_to_list(NAtom);
                  NStr  when is_list(NStr)  -> NStr
                end
            end, DataFrame).

df_values(DataFrame) ->
  lists:map(fun({_Name, Type, Values}) ->
                {Type, Values}
            end, DataFrame).

df_row_names(DataFrame) ->
  {_Name, _Type, Values} = hd(DataFrame),
  N = length(Values),
  lists:seq(1, N).
