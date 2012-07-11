-module(rserve).

-behaviour(application).


%%%_* Exports ------------------------------------------------------------------

%% External API
-export([ close/1
        , eval/2
        , open/0
        , open/1
        , open/2
        ]).

%% application callbacks
-export([ start/2
        , stop/1
        ]).


%%%_* Local definitions --------------------------------------------------------
-define(default_host, "localhost").
-define(default_port, 6311).


%%%_* External API -------------------------------------------------------------
-spec close(pid()) -> ok | {error, term()}.
close(Pid) ->
  supervisor:delete_child(erserve_sup, Pid).

-spec eval(pid(), string()) -> {ok, term()} | {error, term()}.
eval(Pid, Expr) ->
  gen_server:call(Pid, {eval, Expr}).

-spec open() -> pid().
open() ->
  open(?default_host, ?default_port).

-spec open(string()) -> pid().
open(Host) ->
  open(Host, ?default_port).

-spec open(string(), pos_integer()) -> pid().
open(Host, Port) ->
  Args = [ {host, Host}
         , {port, Port}
         ],
  {ok, Pid} = supervisor:start_child(erserve_sup, Args),
  Pid.


%%%_* application callbacks ----------------------------------------------------
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
  supervisor:start_link({local, erserve_sup}, erserve_sup, []).

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
