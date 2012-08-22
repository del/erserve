# erserve - An Erlang/Rserve communication application

## Introduction

### Rserve

[Rserve](http://www.rforge.net/Rserve/) is a TCP/IP server running in
[R](http://www.r-project.org/), allowing interfacing to R from many
languages without explicitly initialising R or linking with it.

### erserve

erserve is an Erlang application that implements the communication
with Rserve, making it possible to make calls to R from Erlang, and to
receive data back.

The interface is very simple, and the functionality implemented is at
this point limited, but includes the most common and useful data
types.


## Quickstart

1. Download and install [R](http://www.r-project.org/).

2. Install [Rserve](http://www.rforge.net/Rserve/), which can be
   easily done using R's package system:
```R
install.packages('Rserve')
```

3. Start the Rserve server in R:
```R
library(Rserve)
Rserve()
```

4. Open a terminal and clone the erserve git library:
```
git clone https://github.com/del/erserve.git
```

5. Compile erserve:
```
cd erserve
./rebar compile
```

6. Start an erlang node with erserve in its path:
```
erl -pa ebin/
```

7. Start the erserve application and connect to your Rserve
```erlang
application:start(erserve).
Conn = erserve:open("localhost", 6311).
```

8. Send a message to R to verify the connection works:
```erlang
{ok, Rdata} = erserve:eval(Conn, "c(1, 2, 3)"),
erserve:type(Rdata),  % xt_array_double
erserve:parse(Rdata). % [1.0,2.0,3.0]
```


## Connections

An erserve connection is opened using one of functions `open/0`, `open/1` or `open/2`, where the
arguments, if given, are hostname and port:
```erlang
Conn1 = open(),                 %% open("localhost", 6311)
Conn2 = open("somehost"),       %% open("somehost",  6311)
Conn3 = open("somehost", 1163).
```
To close a connection, simply send it to `close/1`:
```erlang
ok = close(Conn).
```


## Issuing R commands

erserve supports two ways of running R commands: `eval_void/2` and `eval/2`.
The difference is that `eval_void/2` only receives an `ok` or `{error, ErrorCode, Reason}` as reply,
whereas `eval/2` returns `{ok, Rdata}` or `{error, ErrorCode, Reason}`.

`eval_void(Conn, Expr)` is used when you're issuing a command for the reason of side effects:
```erlang
ok = erserve:eval_void(Conn, "some.var <- 42").
```
whereas `eval(Conn, Expr)` is used when you wish to receive a reply from R. The return is in an
internal format which **should not** be matched on, since it is subject to change. To use the
returned data, make use of the `type/1` and `parse/1` functions:
```erlang
{ok, Rdata} = erserve:eval(Conn, "some.var"),
[42.0]      = case erserve:type(Rdata) of
                xt_array_double -> erserve:parse(Rdata);
                _OtherType      -> error
              end.
```


## Sending/receiving data

It's possible to upload a variable to R directly in binary format, to avoid having to create
expression strings for everything. To do this, use `set_variable/4`, which has the signature
`set_variable(Conn, Name, Type, Value)`.
```erlang
ok                     = erserve:set_variable(Conn, "some.var", xt_array_str, ["bla", "bla"]),
{ok, Rdata}            = erserve:eval(Conn, "some.var"),
xt_array_str           = erserve:type(Rdata),
[<<"bla">>, <<"bla">>] = erserve:parse(Rdata).
```

Note that erserve outputs all strings in binary format.

At the moment, uploading of variables supports the simple R types `xt_str`, `xt_array_double`,
`xt_array_int` and `xt_array_str`. On top of this, it also supports the more advanced formats
`xt_vector` and `dataframe`.

An `xt_vector` consists of a list of tuples `{Type, Value}` where `Type` is one of the just
mentioned types.

A dataframe is a list of tuples `{Name, Type, Value}`. Each such tuple generates a column in the
resulting dataframe, where `Name` is a string that becomes the column's name, and `Type` and
`Value` are as described before.

Some examples of variable uploading:
```erlang
ok = erserve:set_variable(Conn, "some.var", xt_str,          "hello world"),
ok = erserve:set_variable(Conn, "some.var", xt_array_double, [1.1, 2.2, 3.3]),
ok = erserve:set_variable(Conn, "some.var", xt_array_int,    [1, 2, 3]),
ok = erserve:set_variable(Conn, "some.var", xt_array_str,    ["hello", "world"]),
ok = erserve:set_variable(Conn, "some.var", xt_vector,       [ {xt_array_str, ["a", "b"]}
                                                             , {xt_array_int, [1, 3, 5]} ]),
ok = erserve:set_variable(Conn, "some.var", dataframe,       [ {"Letters", xt_array_str, ["a", "b"]}
                                                             , {"Numbers", xt_array_int, [1, 3]} ]).
```
