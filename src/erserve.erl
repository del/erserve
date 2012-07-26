-module(erserve).

-behaviour(application).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ close/1
        , eval/2
        , eval_void/2
        , open/1
        , open/2
        , open/3
        , set_variable/4
        ]).

%% application callbacks
-export([ start/2
        , stop/1
        ]).


%%%_* Types --------------------------------------------------------------------
-type type() :: int
              | double
              | string
              | array_int
              | array_double
              | array_string.


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* Connection handling ------------------------------------------------------
-spec close(term()) -> ok | {error, term()}.
close(Name) ->
  Pid = gen_server:call(Name, get_pid),
  supervisor:terminate_child(erserve_sup, Pid).

-spec open(term()) -> string().
open(Name) ->
  open(Name, ?default_host).

-spec open(term(), string()) -> string().
open(Name, Host) ->
  open(Name, Host, ?default_port).

-spec open(term(), string(), pos_integer()) -> string().
open(Name, Host, Port) ->
  {ok, _Pid} = supervisor:start_child(erserve_sup, [Name, Host, Port]),
  ok.


%%%_* Commands -----------------------------------------------------------------
-spec eval(term(), string()) -> {ok, term()} | {error, term(), binary()}.
eval(Name, Expr) ->
  gen_server:call(Name, {eval, Expr}).

-spec eval_void(term(), string()) -> ok | {error, term(), binary()}.
eval_void(Name, Expr) ->
  gen_server:call(Name, {eval_void, Expr}).

-spec set_variable(term(), string() | atom(), type(), term()) ->
                      {ok, term()} | {error, term(), binary()}.
set_variable(Name, VarName, Type, Value) ->
  gen_server:call(Name, {set_variable, VarName, Type, Value}).


%%%_* application callbacks ----------------------------------------------------
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
  supervisor:start_link({local, erserve_sup}, erserve_sup, []).

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
