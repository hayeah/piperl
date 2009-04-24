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
    exit(success).

piperl_slave_test() ->
    {ok,Slave}=piperl_slave:start_link(echo_exe()),
    Msg = echo_msg(),
    piperl_slave:send(Slave,Msg),
    piperl_slave:send(Slave,Msg),
    piperl_slave:send(Slave,Msg),
    Rs = get_msgs(),
    ?assertEqual(3,  length(Rs)),
    lists:foreach(fun ({_From,Bin}) -> ?assertEqual(Msg#msg.data,Bin) end, Rs).

piperl_master_test() ->
    % start three slave instances
    {ok,Master} = piperl_master:start_link(echo_exe(),[{node(),3}]),
    Slaves = piperl_master:get_slaves(Master),
    msg_slaves(Slaves).

piperl_test() ->
    {ok,Pid} = piperl:start_link(),
    piperl:open(Pid,echo,echo_exe(),[{node(),3}]),
    not_found = piperl:find_slaves(Pid,not_echo),
    Slaves = piperl:find_slaves(Pid,echo),
    ?assertEqual(3,length(Slaves)),
    msg_slaves(Slaves).

piperl_client_test() ->
    {ok,Piperl} = piperl:start_link(),
    piperl:open(Piperl,echo,echo_exe(),[{node(),3}]),
    {ok,Client} = piperl_client:start_link(Piperl),
    Bin = <<"client test">>,
    piperl_client:send(Client,echo,echo_msg(Bin)),
    piperl_client:send(Client,echo,echo_msg(Bin)),
    piperl_client:send(Client,echo,echo_msg(Bin)),
    [begin ?assertMatch(Bin,Bin2) end || {_From,Bin2} <- get_msgs()].

msg_slaves(Slaves) when is_list(Slaves) ->
    Msg = echo_msg(),
    [begin
         piperl_slave:send(Slave,Msg)
     end
     || Slave <- Slaves],
    Rs = get_msgs(),
    NMsgs = length(Slaves),
    ?assertEqual(NMsgs,length(Rs)),
    [?assertEqual(Msg#msg.data,Bin) || {_From,Bin} <-  Rs].

get_msgs() ->
    timer:sleep(100),
    get_msgs([]).
get_msgs(Acc) ->
    case get_msg() of
        none -> Acc;
        R -> get_msgs([R|Acc])
    end.

-spec get_msg() -> {string(),string()} | none.
get_msg() ->
    receive
        {slave_out,#msg{data=Bin}} ->
            {match,[From,Data]} = re:run(Bin,"(\\d+)==(.*)",[{capture,[1,2],binary}]),
            {From,Data}
    after 0 -> none
    end.
    

echo_exe() ->
    #exe{bin="ruby test/echo.rb"}.

echo_msg() ->
    echo_msg(<<"echo msg">>).
echo_msg(Bin) ->
    #msg{handler=self(),data=Bin}.
