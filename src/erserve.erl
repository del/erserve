-module(erserve).

-behaviour(application).
-behaviour(supervisor).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ close/1
        , eval/2
        , eval_void/2
        , open/0
        , open/1
        , open/2
        , set_variable/4
        ]).

%% application callbacks
-export([ start/2
        , stop/1
        ]).

%% supervisor callbacks
-export([ init/1
        ]).


%%%_* Types --------------------------------------------------------------------
-opaque connection()  :: gen_tcp:socket().
-type   expression()  :: string().
-type   r_data()      :: integer()
                       | float()
                       | binary()
                       | sexp()
                       | list_tag()
                       | data_frame()
                       | [ r_data() ].
-type   sexp()        :: { type(), r_data() }.
-type   list_tag()    :: [ { sexp(), sexp() } ].
-type   data_frame()  :: [ { column_name(), r_data() } ].
-type   column_name() :: string().
-type   error_code()  :: atom().
-type   type()        :: int
                       | double
                       | string
                       | sexp
                       | array_int
                       | array_double
                       | array_string
                       | array_bool
                       | list_tag
                       | symname
                       | vector
                       | data_frame.

-export_type([ connection/0
             , column_name/0
             , error_code/0
             , expression/0
             , data_frame/0
             , list_tag/0
             , r_data/0
             , sexp/0
             , type/0
             ]).


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* Connection handling ------------------------------------------------------
-spec close(connection()) -> ok.
close(Conn) ->
  gen_tcp:close(Conn).

-spec open() -> connection().
open() ->
  open(?default_host, ?default_port).

-spec open(Host :: string()) -> string().
open(Host) ->
  open(Host, ?default_port).

-spec open(Name :: string(), Port :: pos_integer()) -> connection().
open(Host, Port) ->
  {ok, Conn} = gen_tcp:connect(Host, Port, [ {active, false}
                                           , binary
                                           , {packet, raw}
                                           ]),
  try
    ok = erserve_comms:receive_connection_ack(Conn),
    Conn
  catch
    _:_ ->
      close(Conn),
      throw(invalid_ack)
  end.


%%%_* Commands -----------------------------------------------------------------
-spec eval(connection(), expression()) ->
              {ok, r_data()} | {error, error_code(), term()}.
eval(Conn, Expr) ->
  case erserve_comms:send_message(Conn, eval, Expr) of
    ok             ->
      erserve_comms:receive_reply(Conn);
    {error, Error} ->
      {error, tcp, Error}
  end.

-spec eval_void(connection(), expression()) ->
                   ok | {error, error_code(), term()}.
eval_void(Conn, Expr) ->
  case erserve_comms:send_message(Conn, eval_void, Expr) of
    ok             ->
      case erserve_comms:receive_reply(Conn) of
        {ok, []}               -> ok;
        {error, AckCode, Rest} -> {error, AckCode, Rest}
      end;
    {error, Error} ->
      {error, tcp, Error}
  end.

-spec set_variable(connection(), string() | atom(), type(), r_data()) ->
                      {ok, r_data()} | {error, error_code(), term()}.
set_variable(Conn, Name, Type,   Value) when is_atom(Name) ->
  set_variable(Conn, atom_to_list(Name), Type, Value);
set_variable(Conn, Name, int,    Value)                    ->
  set_variable(Conn, Name, array_int, [Value]);
set_variable(Conn, Name, double, Value)                    ->
  set_variable(Conn, Name, array_double, [Value]);
set_variable(Conn, Name, Type, Value)                      ->
  case erserve_comms:send_message(Conn, {set_variable, Type}, {Name, Value}) of
    ok             ->
      erserve_comms:receive_reply(Conn);
    {error, Error} ->
      {error, tcp, Error}
  end.


%%%_* application callbacks ----------------------------------------------------
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(term()) -> ok.
stop(_State) ->
  ok.


%%%_* supervisor callbacks -----------------------------------------------------
-spec init([]) -> term().
init([]) ->
  { ok
  , { { one_for_one, 2, 60 }
    , []
    }
  }.
