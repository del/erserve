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
   
       install.packages('Rserve')

3. Start the Rserve server in R:

       library(Rserve)  
       Rserve()

4. Clone the erserve git library:

       git clone https://github.com/del/erserve.git

5. Compile erserve:

       cd erserve  
       ./rebar compile

6. Start an erlang node with erserve in its path:

       erl -pa ebin/

7. Start the erserve application and connect to your Rserve

       application:start(erserve).  
       erserve:open(my_conn, "localhost", 6311).

8. Send a message to R to verify the connection works:

       erserve:eval(my_conn, "c(1, 2, 3)").  
       % {ok,[{{array,double},[1.0,2.0,3.0]}]}