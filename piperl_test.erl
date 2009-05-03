-module(piperl_test).
-compile(export_all).
-define(EUNIT_NOAUTO,true).
-include_lib("eunit/include/eunit.hrl").
-include("piperl.hrl").


test() ->
  piperl_slave_test(),
  piperl_master_test(),
  piperl_test(),
  piperl_client_test(),
  piperl_tcp_server_test(),
  exit(success).

piperl_slave_test() ->
  {ok,Slave}=piperl_slave:start_link(echo_exe()),
  Bin = <<"slave test">>,
  Rs = msg_slaves([Slave,Slave,Slave],Bin),
  ?assertEqual(3,length(Rs)),
  [begin ?assertMatch(Bin,Bin2) end || {_From,Bin2} <- Rs],
  shutdown(Slave),
  {ok,Slave2}=piperl_slave:start_link(echo_exe()),
  %% try failure
  try
    unlink(Slave2),
    ?debugMsg("Expects Error"),
    piperl_slave:send(Slave2,echo_msg(<<"fail">>,100)),
    ?assertMatch([{error,epipe}],get_msgs(200)),
    %% Slave2 should be dead
    ?assertExit(_,piperl_slave:send(Slave2,echo_msg()))
    after
      catch shutdown(Slave2)
    end,
  {ok,Slave3}=piperl_slave:start_link(echo_exe()),
  %% timeout
  try
    unlink(Slave3),
    ?debugMsg("Expects Error"),
    piperl_slave:send(Slave3,echo_msg(<<"timeout">>,100)),
    ?assertMatch([{error,timeout}],get_msgs(200)),
    %% Slave3 should be dead
    ?assertExit(_,piperl_slave:send(Slave3,echo_msg()))
    after
      catch shutdown(Slave3)
    end,
  %% success once, then die
  {ok,Slave4}=piperl_slave:start_link(echo_exe()),
  try
    unlink(Slave4),
    ?debugMsg("Expects Error"),
    piperl_slave:send(Slave4,echo_msg(<<"fail-after">>,100)),
    ?assertMatch([{_,<<"swan-song">>}],get_msgs(200)),
    %% by now, the process is dead
    piperl_slave:send(Slave4,echo_msg(<<"encore">>)),
    ?assertMatch([{error,epipe}],get_msgs(100)),
    %% Slave4 should be dead
    ?assertExit(_,piperl_slave:send(Slave4,echo_msg()))
    after
      catch shutdown(Slave4)
    end,
  ok.

piperl_master_test() ->
  %% start three slave instances
  {ok,Master} = piperl_master:start_link(echo_exe(),[{node(),3}]),
  Slaves = piperl_master:get_slaves(Master),
  ?assertEqual(3,length(Slaves)),
  %% kill all slaves and wait for supervisor to restart them
  %%
  %% we expect the slaves to be different erlang
  %% pids, and the results returned from different
  %% unix pids.
  UPids = [UPid || {UPid,_} <- msg_slaves(Slaves,<<"master test 1">>)],
  ?assertEqual(3,length(UPids)),
  [exit(Slave,die_die_die) || Slave <- Slaves],
  %% let supervisor restart processes
  timer:sleep(100),
  Slaves2 = piperl_master:get_slaves(Master),
  ?assertEqual(3,length(Slaves2)),
  ?assert(ordsets:is_disjoint(
            ordsets:from_list(Slaves),
            ordsets:from_list(Slaves2))),
  UPids2 = [UPid || {UPid,_} <- msg_slaves(Slaves2,<<"master test 2">>)],
  ?assertEqual(3,length(UPids2)),
  ?assert(ordsets:is_disjoint(
            ordsets:from_list(UPids),
            ordsets:from_list(UPids2))),
  shutdown(Master).

piperl_test() ->
  {ok,Piperl} = piperl:start(),
  link(Piperl),
  piperl:open(Piperl,echo,echo_exe(),[{node(),3}]),
  not_found = piperl:find_slaves(Piperl,not_echo),
  Slaves = piperl:find_slaves(Piperl,echo),
  ?assertEqual(3,length(Slaves)),
  ?assertEqual(3,length(msg_slaves(Slaves,<<"piperl test">>))),
  shutdown(Piperl).

piperl_client_test() ->
  {ok,Piperl} = piperl:start(),
  link(Piperl),
  piperl:open(Piperl,echo,echo_exe(),[{node(),3}]),
  {ok,Client} = piperl_client:start_link(Piperl),
  Bin = <<"client test">>,
  piperl_client:send(Client,echo,echo_msg(Bin)),
  piperl_client:send(Client,echo,echo_msg(Bin)),
  piperl_client:send(Client,echo,echo_msg(Bin)),
  [begin ?assertMatch(Bin,Bin2) end || {_From,Bin2} <- get_msgs()],
  shutdown(Piperl).

piperl_tcp_server_test() ->
  {ok,Piperl} = piperl:start(),
  link(Piperl),
  piperl:open(Piperl,echo,echo_exe(),[{node(),1}]),
  TcpServer = piperl_tcp_server:start(9876,Piperl),
  link(TcpServer),
  {ok,Timer} = timer:exit_after(10000,self(),tcp_timeout),
  {ok,Socket} = gen_tcp:connect("localhost",9876,[binary,{active,false}]),
  gen_tcp:send(Socket,<<"{'send'  'echo' 7~tcp_msg~}$\n\n\n  {'send' 'echo' \"tcp_msg2\"}$\n">>),
  {Bin,Excess} = piperl_util:decode_ubf_stream(
                   fun () -> {ok,TBin} = gen_tcp:recv(Socket,0), TBin end),
  {Bin2,_} = piperl_util:decode_ubf_stream(
               fun () -> {ok,TBin} = gen_tcp:recv(Socket,0), TBin end,
               Excess),
  ?assertMatch({_From,<<"tcp_msg">>},parse_msg(Bin)),
  ?assertMatch({_From,<<"tcp_msg2">>},parse_msg(Bin2)),
  timer:cancel(Timer),
  shutdown(Piperl).

msg_slaves(Slaves,Bin) when is_list(Slaves) ->
  msg_slaves(Slaves,Bin,100).
msg_slaves(Slaves,Bin,Wait) when is_list(Slaves), is_integer(Wait) ->
  Msg = echo_msg(Bin,Wait), %% stipulates that this particular message timeouts after Wait.
  [piperl_slave:send(Slave,Msg) || Slave <- Slaves],
  get_msgs(Wait).

get_msgs() ->
  get_msgs(100).
get_msgs(Wait) when is_integer(Wait) ->
  timer:sleep(Wait),
  collect_msgs([]).
collect_msgs(Acc) ->
  case collect_msg() of
    none -> Acc;
    R -> collect_msgs([R|Acc])
  end.

-spec collect_msg() -> {string(),string()} | none.
collect_msg() ->
  receive
    {out,#msg{data=Bin}} -> parse_msg(Bin);
    {out,#err_msg{reason=R}} -> {error,R}
  after 0 -> none
  end.

parse_msg(Bin) ->
  {match,[From,Data]} = re:run(Bin,"(\\d+)==(.*)",[{capture,[1,2],binary}]),
  {list_to_integer(binary_to_list(From)),Data}.


echo_exe() ->
  #exe{bin="ruby test/echo.rb"}.

echo_msg() ->
  echo_msg(<<"echo msg">>).
echo_msg(Bin) ->
  echo_msg(Bin,100).
echo_msg(Bin,Timeout) ->
  #msg{handler=self(),data=Bin,timeout=Timeout}.

shutdown(Pid) ->
  unlink(Pid),
  exit(Pid,shutdown).
