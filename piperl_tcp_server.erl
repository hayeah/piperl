-module(piperl_tcp_server).
-include("piperl.hrl").
-export([start/2]).

-spec start(integer(),pid()) -> pid().
start(Port,Piperl) when is_integer(Port), is_pid(Piperl)->
  spawn(fun () ->
            link(Piperl), % piperl dies, then server should die.
            socket_listen(Port,Piperl)
        end).

socket_listen(Port,Piperl) ->
  {ok,LSock} = gen_tcp:listen(Port,[binary,{active,false}]),
  try socket_accept(LSock,Piperl)
      after gen_tcp:close(LSock) %% closes socket when server dies
      end.

socket_accept(LSock,Piperl) ->
  case gen_tcp:accept(LSock) of
    {ok,Sock} ->
      %% link SocketWriter, SocketReader, Client together
      %% %% Client dies when Piperl dies, or when tcp socket closes.
      SocketWriter =
        spawn(
          fun () -> socket_write_loop(Sock)
          end),
      SocketReader =
        spawn(
          fun () ->
              receive init -> ok end,
              link(SocketWriter),
              {ok,Client} = piperl_client:start_link(Piperl),
              socket_read_loop(Sock,SocketWriter,Client)
          end),
      gen_tcp:controlling_process(Sock,SocketReader),
      SocketReader ! init,
      socket_accept(LSock,Piperl);
    _ -> exit(listen_socket_closed)
  end.

socket_read_loop(Sock,SocketWriter,Client) ->
  socket_read_loop(Sock,SocketWriter,Client,[]).
socket_read_loop(Sock,SocketWriter,Client,LeftOver) ->
  {UBF,LeftOver2} = piperl_util:decode_ubf_stream(
                      fun () -> socket_receive(Sock) end,LeftOver),
  case UBF of
    {'send',Name,Data} when is_atom(Name) ->
      Bin =
        case Data of
          {'#S',Str} -> iolist_to_binary(Str);
          B -> B
        end,
      piperl_client:send(Client,Name,#msg{data=Bin,handler=SocketWriter})
  end,
  socket_read_loop(Sock,SocketWriter,Client,LeftOver2).

socket_write_loop(Sock) ->
  receive
    {slave_out,#msg{data=Bin}} ->
      gen_tcp:send(Sock,ubf:encode(Bin)),
      socket_write_loop(Sock)
  end.

socket_receive(Sock) ->
  inet:setopts(Sock,[{active,once}]),
  check_tcp_open(Sock),
  receive 
    {tcp,Sock,Bin} -> Bin
  end.

check_tcp_open(Sock) ->
  receive
    E={tcp_closed,Sock} -> exit(E)
  after 0 -> ok
  end.


                                                % 
