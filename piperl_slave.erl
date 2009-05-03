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

-spec send(pid(),msg()) -> 'ok' | {'error',_}.
send(SlavePid,Msg=#msg{data=Bin}) when is_binary(Bin) ->
  gen_server:call(SlavePid,{slave_in,Msg}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

%% CONTRACT: spawned unix process should exit on broken pipe
%%
%% The process loop is used to prevent the
%% piperl_slave gen_server instance from
%% blocking. The loop itself processes each
%% message sequentially. (Should prevent blocking,
%% because we want slave to acknowledge receipt of
%% message).
init([Exe]) ->
  P = spawn_link(
        fun () ->
            Port = open_port({spawn,Exe#exe.bin},[stream,binary]),
            process_loop(#slave_processor{port=Port})
        end),
  {ok,#slave{pid=self(),processor=P}}.

handle_call({slave_in,Msg=#msg{}},_From,S) ->
  S#slave.processor ! Msg,
  {reply,ok,S}.

handle_cast(_,_) ->
  exit(undefined).

handle_info(_,_) ->
  exit(undefined).

terminate(Reason,_S) ->
  exit(Reason).

code_change(_OldVsn,S,_Extra) ->
  {ok,S}.

process_loop(P) ->
  receive
    Msg=#msg{handler=Handler,data=Bin,timeout=MsgTimeout} ->
      %% if the message specifies a timeout, use that.
      Timeout = case MsgTimeout of
                  undefined -> P#slave_processor.timeout;
                  _ -> MsgTimeout
                end,
      %% augment processor with the current message being processed. Throw away later.
      P2 = P#slave_processor{msg=Msg},
      write(P2,Bin),
      R = case read(P2,Timeout) of
            %% this is an error message the slave produced
            {error,Reason} -> #err_msg{reason=Reason,seq=Msg#msg.seq};
            RBin -> Msg#msg{data=RBin}
          end,
      reply(Handler,R)
  end,
  process_loop(P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions

write(P,Bin) when is_binary(Bin) ->
  %% data disappears if port is closed (which is fine)
  Port = P#slave_processor.port,
  try
    erlang:port_command(Port,ubf:encode(Bin))
  catch error:badarg -> die(P,epipe)
  end.

read(P,Timeout) ->
  case piperl_util:decode_ubf_stream(fun () -> receive_bin(P,Timeout) end) of
    {Data,[]} -> Data;
    {Data,LeftOver} -> erlang:error({left_over,Data,LeftOver})
  end.

receive_bin(P,Timeout) ->
  Port = P#slave_processor.port,
  receive
    {Port,{data,Bin}} -> Bin
  after Timeout ->
      %% can't know if we are timing out because pipe is broken. Try to close it.
      try port_close(Port)
      catch error:badarg -> die(P,epipe)
      end,
      die(P,timeout)
  end.

die(P,Reason) ->
  #msg{handler=Handler,seq=Seq} = P#slave_processor.msg,
  reply(Handler,#err_msg{reason=Reason,seq=Seq}),
  %% TODO flush pending messages in mailbox
  %% %% pending messages should be retried.
  %% flush_pending(),
  erlang:error(Reason).

reply(Handler,Term) ->
  Handler ! {out,Term}.
