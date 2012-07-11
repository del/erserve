-module(erserve).

-behaviour(application).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ close/1
        , eval/2
        , open/1
        , open/2
        , open/3
        ]).

%% application callbacks
-export([ start/2
        , stop/1
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* External API -------------------------------------------------------------
-spec close(string()) -> ok | {error, term()}.
close(Name) ->
  Pid = gen_server:call(Name, get_pid),
  supervisor:terminate_child(erserve_sup, Pid).

-spec eval(string(), string()) -> {ok, term()} | {error, term()}.
eval(Name, Expr) ->
  gen_server:call(Name, {eval, Expr}).

-spec open(string()) -> string().
open(Name) ->
  open(Name, ?default_host).

-spec open(string(), string()) -> string().
open(Name, Host) ->
  open(Name, Host, ?default_port).

-spec open(string(), string(), pos_integer()) -> string().
open(Name, Host, Port) ->
  {ok, _Pid} = supervisor:start_child(erserve_sup, [Name, Host, Port]),
  ok.


%%%_* application callbacks ----------------------------------------------------
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
  supervisor:start_link({local, erserve_sup}, erserve_sup, []).

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
