-module(piperl).
-include("piperl.hrl").
-behaviour(gen_server).


-export([start_link/0,
         open/4,
         find/2,
         find_slaves/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(piperl,
        {pipes %% dict of pipe names to piperl_master pids
        }).
-type piperl() :: #piperl{pipes::dict()}.


-type hosts_spec() :: [{node(),non_neg_integer()} | node()].

-spec start_link() -> {'ok',pid()}.
start_link() ->
    gen_server:start_link(?MODULE,[],[]).

-spec open(atom() | pid(), atom(),exe(),hosts_spec()) -> 'ok' | {'error',_}.
open(Pid,Name,Exe,Hosts) ->
    gen_server:call(Pid,{open,Name,Exe,Hosts}).

%close

-spec find(atom() | pid(),atom()) -> pid() | not_found.
find(PiperlPid,Name) ->
    gen_server:call(PiperlPid,{find,Name}).

-spec find_slaves(atom() | pid(),atom()) -> [pid()] | not_found.
find_slaves(PiperlPid,Name) ->
    case piperl:find(PiperlPid,Name) of
        not_found -> not_found;
        Master -> piperl_master:get_slaves(Master)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_sever call backs

-spec init([non_neg_integer()]) -> {ok,piperl()}.
init([]) ->
    {ok,#piperl{pipes=dict:new()}}.

%% gen_server API internal to piperl
handle_call({open,Name,Exe,Hosts},_From,S) ->
    case find_master(S,Name) of
        not_found ->
            case piperl_master:start_link(Exe,Hosts) of
                {ok,Pid} ->
                    Pipes2 = dict:store(Name,Pid,S#piperl.pipes),
                    {reply,ok,S#piperl{pipes=Pipes2}};
                Err -> {reply,{error,Err},S}
            end;
        _ -> {reply,{error,pipe_exists,Name},S}
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

-spec find_master(piperl(),atom()) -> pid() | 'not_found'.
find_master(S,Name) ->
    case dict:find(Name,S#piperl.pipes) of
        {ok,Pid} -> Pid;
        error -> not_found
    end.
    
