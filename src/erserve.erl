%%%-----------------------------------------------------------------------------
%%% @doc erserve allows Erlang code to call R code via Rserve. The basic usage
%%%      is to open a connection and send one or more commands to it, then use
%%%      the type/1 and parse/1 functions to transfer the data from internal
%%%      representation to "Erlangy" formats.
%%%
%%% @reference For more information, see <a href="http://r-project.org">R</a>
%%%            and <a href="http://rforge.net/Rserve/">Rserve</a>.
%%%
%%% @author Daniel Eliasson <daniel@danieleliasson.com>
%%% @copyright 2012 Daniel Eliasson; Apache 2.0 license -- see LICENSE file
%%% @end------------------------------------------------------------------------
-module(erserve).

-behaviour(application).
-behaviour(supervisor).


%%%_* Exports ------------------------------------------------------------------

%% Connection handling
-export([ close/1
        , open/0
        , open/1
        , open/2
        ]).

%% R commands
-export([ eval/2
        , eval_void/2
        , set_variable/4
        ]).

%% Data parsing
-export([ type/1
        , parse/1
        ]).

%% application callbacks
-export([ start/2
        , stop/1
        ]).

%% supervisor callbacks
-export([ init/1
        ]).


%%%_* Types --------------------------------------------------------------------
-opaque connection()    :: gen_tcp:socket().
-type   r_expression()  :: string().
-opaque r_data()        :: {xt_str,          binary()}
                         | {xt_array_bool,   [ boolean() ]}
                         | {xt_array_double, [ float() ]}
                         | {xt_array_int,    [ integer() ]}
                         | {xt_array_str,    [ binary() ]}
                         | {xt_list_tag,     [ {r_data(), r_data()} ]}
                         | {xt_vector,       [ r_data() ]}
                         | {xt_has_attr,     { tag(), r_data() }}.
-opaque tag()           :: [ {{xt_str, string()}, r_data()} ].
-type   r_type()        :: xt_str
                         | xt_array_bool
                         | xt_array_double
                         | xt_array_int
                         | xt_array_str
                         | xt_list_tag
                         | xt_vector
                         | dataframe
                         | list
                         | unsupported.
-opaque r_df()         :: { xt_has_attr
                          , { {xt_list_tag, [ r_data() ]}
                            , {xt_vector,   [ r_data() ]}
                            }
                          }.
-type   r_class()       :: string().
-type   untagged_data() :: float()
                         | integer()
                         | binary()
                         | [ float() ]
                         | [ integer() ]
                         | [ binary() ]
                         | [ untagged_data() ]
                         | df().
-type   df()           :: [ { Name :: string(), r_data() } ].
-type   error_code()   :: atom().
-type   type()         :: int
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
                        | dataframe.

-export_type([ connection/0
             , column_name/0
             , error_code/0
             , r_class/0
             , r_data/0
             , r_expression/0
             , r_df/0
             , r_expression/0
             , r_type/0
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


%%%_* R commands ---------------------------------------------------------------
-spec eval(connection(), r_expression()) ->
              {ok, r_data()} | {error, error_code(), term()}.
eval(Conn, Expr) ->
  case erserve_comms:send_message(Conn, eval, Expr) of
    ok             ->
      erserve_comms:receive_reply(Conn);
    {error, Error} ->
      close(Conn),
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
      close(Conn),
      {error, tcp, Error}
  end.

-spec set_variable(connection(), string() | atom(), type(), untagged_data()) ->
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
      close(Conn),
      {error, tcp, Error}
  end.


%%%_* Data parsing -------------------------------------------------------------
-spec type(r_data()) -> r_type().
type(Rdata) ->
  erserve_data:type(Rdata).

-spec parse(r_data()) -> untagged_data().
parse(Rdata) ->
  erserve_data:parse(Rdata).


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
