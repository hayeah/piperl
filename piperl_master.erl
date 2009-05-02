% master starts a supervisor that starts and
% restarts slaves. Master and supervisor are
% linked (one dies, both die).
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

-spec start_link(#exe{}) -> {'ok',pid()} | {'error',_}.
start_link(Exe) ->
  start_link(Exe,[{node(),1}]).
%% don't expose the second head until remote spawn is supported
-spec start_link(#exe{},hosts_spec()) -> {'ok',pid()} | {'error',_}.
start_link(Exe,Hosts) ->
  Hosts2 = 
    [case Node of
       {Name,Num} when is_atom(Name), is_integer(Num) ->
         Node;
       Name when is_atom(Name) ->
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
  {ok,Supervisor} = piperl_slaves_supervisor:start_link(),
  M = #master{exe=Exe,hosts=Hosts,supervisor=Supervisor},
  _ = [{Node,
        case supervisor:start_child(Supervisor,[Node,Exe]) of
          {ok,_Slave} -> ok;
          {ok,_Slave,_Info} -> ok;
          {error,E} -> erlang:error(E)
        end}
       || {Node,Num} <- Hosts,
          _ <- lists:seq(1,Num)],
  {ok,M}.

handle_cast(_Arg,_M) ->
  exit(undefined).
handle_info(_,_) ->
  exit(undefined).

handle_call(get_slaves,{Pid,_Tag},M) ->
  Clients2 = ordsets:add_element(Pid,M#master.clients),
  M2 = M#master{clients=Clients2},
  {reply, supervisor_slaves(M2), M2}.

terminate(Reason,_M) ->
  exit(Reason).

code_change(_OldVsn,M,_Extra) ->
  {ok,M}.

supervisor_slaves(M) ->
  [Slave || {_,Slave,_,_} <- supervisor:which_children(M#master.supervisor)].

%% TODO master only needs to notify clients when new instances are added.
%% %% client notices dead processes (with monitor)
%% notify_changes(M,Clients)
