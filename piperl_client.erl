-module(piperl_client).
-include("piperl.hrl").


-export([start_link/1,
         send/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).

-record(client,
        {piperl,
         slavess=dict:new(),
         seq=1 %% sequence number
        }).

-type client() :: #client{piperl::pid() | atom(),slavess::dict()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

-spec start_link(atom() | pid()) -> {ok,pid()}.
start_link(PiperlPid) when is_pid(PiperlPid) ->
  gen_server:start_link(?MODULE,[PiperlPid],[]).

-spec send(pid(),atom(),msg()) -> 'ok' | {'error',_}.
send(ClientPid,PipeName,Msg)
  when is_pid(ClientPid), is_atom(PipeName), is_record(Msg,msg) ->
  %% this call is actually asychronous. But I want
  %% gen_server to report error when ClientPid is gone.
  gen_server:call(ClientPid,{send,PipeName,Msg}).

%% sync()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server
-spec init(_) -> {'ok',client()}.
init([PiperlPid]) ->
  {ok,#client{piperl=PiperlPid}}.

handle_call({send,PipeName,Msg},_From,S) ->
  {Slave,S2} = choose_slave(S,PipeName),
  %% assign message sequence number as token if none is used.
  piperl_slave:send(Slave,Msg),
  {reply,ok,S2}.

handle_cast(_,_) ->
  exit(undefined).

handle_info({slave_out,Msg=#msg{handler=Handler}},S) ->
  Handler ! Msg,
  {noreply,S}.

terminate(Reason,_S) ->
  exit(Reason).

code_change(_OldVsn,S,_Extra) ->
  {ok,S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

%% round robin
-spec choose_slave(client(),atom()) -> {pid(),client()}.
choose_slave(S,PipeName) ->
  case dict:find(PipeName,S#client.slavess) of
    {ok,{N,Slaves}} ->
      N2 = (N rem size(Slaves))+1,
      NewDict = dict:store(PipeName,{N2,Slaves},S#client.slavess),
      {element(N2,Slaves),S#client{slavess=NewDict}};
    error ->
      Slaves = list_to_tuple(piperl:find_slaves(S#client.piperl,PipeName)),
      NewDict = dict:store(PipeName,{1,Slaves},S#client.slavess),
      {element(1,Slaves),S#client{slavess=NewDict}}
  end.
