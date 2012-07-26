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
-opaque connection()   :: gen_tcp:socket().
-type   r_expression() :: string().
-type   r_data()       :: integer()
                        | float()
                        | string()
                        | [ integer() ]
                        | [ float() ]
                        | [ string() ]
                        | [ { r_data(), r_data() } ].
-type   error_code()   :: atom().
-type   type()         :: int
                        | double
                        | string
                        | array_int
                        | array_double
                        | array_string.

-export_type([ connection/0
             , error_code/0
             , r_data/0
             , r_expression/0
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
-spec eval(connection(), r_expression()) ->
              {ok, r_data()} | {error, error_code(), binary()}.
eval(Conn, Expr) ->
  ok = erserve_comms:send_message(Conn, eval, Expr),
  erserve_comms:receive_reply(Conn).

-spec eval_void(connection(), r_expression()) ->
                   ok | {error, error_code(), binary()}.
eval_void(Conn, Expr) ->
  ok = erserve_comms:send_message(Conn, eval_void, Expr),
  case erserve_comms:receive_reply(Conn) of
    {ok, []}               -> ok;
    {error, AckCode, Rest} -> {error, AckCode, Rest}
  end.

-spec set_variable(connection(), string() | atom(), type(), r_data()) ->
                      {ok, r_data()} | {error, error_code(), binary()}.
set_variable(Conn, Name, Type,   Value) when is_atom(Name) ->
  set_variable(Conn, atom_to_list(Name), Type, Value);
set_variable(Conn, Name, int,    Value)                    ->
  set_variable(Conn, Name, array_int, [Value]);
set_variable(Conn, Name, double, Value)                    ->
  set_variable(Conn, Name, array_double, [Value]);
set_variable(Conn, Name, Type, Value)                      ->
  ok = erserve_comms:send_message(Conn, {set_variable, Type}, {Name, Value}),
  erserve_comms:receive_reply(Conn).


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
