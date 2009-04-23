-module(piperl_test).
-compile(export_all).
-include("piperl.hrl").

test() ->
    test_piperl_slave(),
    test_piperl_master().

%% test_piperl() ->

%%     .


test_piperl_slave() ->
    {ok,Pid1} = piperl_slave:start_link(echo_exe()),
    Msg = echo_msg(),
    piperl_slave:send(Pid1,Msg),
    piperl_slave:send(Pid1,Msg),
    piperl_slave:send(Pid1,Msg),
    Rs = get_msgs(500),
    3 =  length(Rs),
    true = lists:all(fun (Bin) -> Bin == Msg#msg.data end,
                     Rs),
    unlink(Pid1),
    exit(Pid1,done),
    ok.

test_piperl_master() ->
    % start three slave instances
    {ok,Pid} = piperl_master:start_link(echo_exe(),[{node(),3}]),
    Slaves = piperl_master:get_slaves(Pid),
    Msg = echo_msg(),
    [begin
         piperl_slave:send(Slave,Msg),
         piperl_slave:send(Slave,Msg),
         piperl_slave:send(Slave,Msg)
     end
     || Slave <- Slaves],
    Rs = get_msgs(500),
    3 * 3 = length(Rs),
    unlink(Pid),
    exit(Pid,done),
    ok.

get_msgs(Timeout) ->
    get_msgs(Timeout,[]).
get_msgs(Timeout,Acc) ->
    receive
        #msg{data=Bin} -> get_msgs(Timeout,[Bin|Acc])
    after Timeout -> lists:reverse(Acc)
    end.
    

echo_exe() ->
    #exe{bin="ruby test/echo.rb"}.

echo_msg() ->
    #msg{handler=self(),data= <<"a">>}.
