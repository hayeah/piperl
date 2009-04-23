-module(piperl).
-include("piperl.hrl").
-behaviour(gen_server).


-export([start_link/1,
         open/4,
         find/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(piperl,
        {pipes, %% dict of pipe names to piperl_master pids
         port
        }).
-type piperl() :: #piperl{pipes::dict(),port::non_neg_integer()}.


-type hosts_spec() :: [{node(),non_neg_integer()} | node()].

-spec start_link(non_neg_integer()) -> {'ok',pid()}.
start_link(Port) ->
    gen_server:start_link({local,?MODULE},?MODULE,[Port],[]).

-spec open(atom() | pid(), atom(),exe(),hosts_spec()) -> 'ok' | {'error',_}.
open(Pid,Name,Exe,Hosts) ->
    gen_server:call(Pid,{open,Name,Exe,Hosts}).

%close

-spec find(atom() | pid(),atom()) -> pid() | 'error'.
find(PiperlPid,Name) ->
    gen_server:call(PiperlPid,{find,Name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_sever call backs

-spec init([non_neg_integer()]) -> {ok,piperl()}.
init([Port]) ->
    % spawn_link(fun () -> socket_listen(Port) end),
    {ok,#piperl{pipes=dict:new(),port=Port}}.

%% gen_server API internal to piperl
handle_call({open,Name,Exe,_Hosts},_From,S) ->
    case find_master(S,Name) of
        {ok,_} -> {reply,{error,pipe_exists,Name},S};
        error ->
            case piperl_master:start_link(Exe) of
                {ok,Pid} ->
                    Pipes2 = dict:store(Name,Pid,S#piperl.pipes),
                    {reply,ok,S#piperl{pipes=Pipes2}};
                Err -> {reply,{error,Err},S}
            end
    end;
handle_call({find,Name},_From,S) ->
    {reply,find_master(S,Name),S}.

handle_cast(_,_) ->
    exit(unimplemented).
handle_info(_,_) ->
    exit(unimplemented).

terminate(Reason,_S) ->
    exit(Reason).

code_change(_OldVsn,S,_Extra) ->
    {ok,S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

find_master(S,Name) ->
    dict:find(Name,S#piperl.pipes).
    
