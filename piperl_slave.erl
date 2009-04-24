-module(piperl_slave).
-include("piperl.hrl").
%% -include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1,
         send/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

-spec start_link(#exe{}) -> {'ok',pid()} | {'error',_}.
start_link(Exe=#exe{}) ->
  gen_server:start_link(?MODULE,[Exe],[]).

send(SlavePid,Msg) when is_record(Msg,msg) ->
  gen_server:cast(SlavePid,{slave_in,Msg}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

init([Exe]) ->
  %% spawned unix process should exit on broken pipe
  Port = open_port({spawn,Exe#exe.bin},[stream,binary]),
  {ok,#slave{port=Port,pid=self()}}.

handle_cast({slave_in,Msg=#msg{data=Data,handler=Handler}},S) ->
  write(S,Data),
  R = read(S), %% blocks
  Handler ! {slave_out,Msg#msg{data=R}},
  {noreply,S}.

handle_call(_,_,_) ->
  exit(undefined).

handle_info(_,_) ->
  exit(undefined).

terminate(Reason,S) ->
  port_close(S#slave.port),
  exit(Reason).

code_change(_OldVsn,S,_Extra) ->
  {ok,S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions

write(S,Data) when is_list(Data) or is_binary(Data) ->
  Bin = iolist_to_binary(Data),
  erlang:port_command(S#slave.port,ubf:encode(Bin)).

read(S) ->
  case piperl_util:decode_ubf_stream(fun () -> receive_bin(S) end) of
    {Data,[]} -> Data;
    {Data,LeftOver} -> erlang:error({left_over,Data,LeftOver})
  end.

receive_bin(S) ->
  Port = S#slave.port,
  receive
    {Port,{data,Bin}} -> Bin
  after S#slave.timeout -> erlang:error("timeout")
  end.
