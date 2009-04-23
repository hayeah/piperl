-module(piperl_test).
-compile(export_all).
-include("piperl.hrl").

test() ->
    ok=test_piperl_slave(),
    ok=test_piperl_master(),
    ok=test_piperl(),
    exit(success).

test_piperl_slave() ->
    {ok,Pid1} = piperl_slave:start_link(echo_exe()),
    Msg = echo_msg(),
    piperl_slave:send(Pid1,Msg),
    piperl_slave:send(Pid1,Msg),
    piperl_slave:send(Pid1,Msg),
    Rs = get_msgs(500),
    3 = length(Rs),
    true = lists:all(fun (Bin) -> Bin == Msg#msg.data end,
                     Rs),
    ok.

test_piperl_master() ->
    % start three slave instances
    {ok,Pid} = piperl_master:start_link(echo_exe(),[{node(),3}]),
    Slaves = piperl_master:get_slaves(Pid),
    ok = msg_slaves(Slaves).

test_piperl() ->
    {ok,Pid} = piperl:start_link(),
    piperl:open(Pid,echo,echo_exe(),[{node(),3}]),
    not_found = piperl:find_slaves(Pid,not_echo),
    Slaves = piperl:find_slaves(Pid,echo),
    ok = msg_slaves(Slaves).

msg_slaves(Slaves) when is_list(Slaves) ->
    Msg = echo_msg(),
    [begin
         piperl_slave:send(Slave,Msg),
         piperl_slave:send(Slave,Msg),
         piperl_slave:send(Slave,Msg)
     end
     || Slave <- Slaves],
    Rs = get_msgs(500),
    NMsgs = length(Slaves) * 3,
    NMsgs = length(Rs),
    ok.

get_msgs(Timeout) ->
    get_msgs(Timeout,[]).
get_msgs(Timeout,Acc) ->
    receive
        {slave_out,#msg{data=Bin}} -> get_msgs(Timeout,[Bin|Acc])
    after Timeout -> lists:reverse(Acc)
    end.
    

echo_exe() ->
    #exe{bin="ruby test/echo.rb"}.

echo_msg() ->
    #msg{handler=self(),data= <<"a">>}.
