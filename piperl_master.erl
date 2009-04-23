-module(piperl_master).
-include("piperl.hrl").

-export([start_link/1,
         start_link/2,
         get_slaves/1
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
-type hosts_spec() :: [{node(),non_neg_integer()} | node()].

-spec start_link(#exe{}) -> {'ok',pid()}.
start_link(Exe) ->
    start_link(Exe,[{node(),1}]).
%% don't expose the second head until remote spawn is supported
-spec start_link(#exe{},hosts_spec()) -> {'ok',pid()}.
start_link(Exe,Hosts) ->
    Hosts2 = 
        [case Node of
             {Name,Num} when is_atom(Name), is_integer(Num),
                             Name == node() ->
                 Node;
             Name when is_atom(Name),
                       Name == node() ->
                 {Name,1}
         end || Node <- Hosts],
    gen_server:start_link(?MODULE,[{Exe,Hosts2}],[]).

-spec get_slaves(pid()) -> [pid()].
get_slaves(MasterPid) ->
    gen_server:call(MasterPid,get_slaves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

-spec init([{#exe{},[{node(),non_neg_integer()}]}]) -> {'ok',master()}.
init([{Exe,Hosts}]) ->
    Slaves =
        %% TODO generalize this for slave spawning on remote node
        [{Node,
          case piperl_slave:start_link(Exe) of
              {ok,Pid} -> Pid;
              {error,E} -> erlang:error(E)
          end}
         || {Node,Num} <- Hosts,
            _ <- lists:seq(1,Num)],
    {ok,#master{exe=Exe,hosts=Hosts,slaves=Slaves}}.

handle_cast(_Arg,_M) ->
    exit(undefined).

handle_info(_,_M) ->
    exit(undefined).

handle_call(get_slaves,_From,M) ->
    {reply,
     [Pid || {_Node,Pid} <- M#master.slaves],
     M}.

terminate(Reason,_M) ->
    exit(Reason).

code_change(_OldVsn,M,_Extra) ->
    {ok,M}.
