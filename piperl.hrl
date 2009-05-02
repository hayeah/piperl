
-record(pipe,
        {name,
         sequence_number=1, %% the sequence number assigned to next message.
         working_set, %% ets table id for the working set of messages
         cmdss
        }).


-record(exe,
        {cd,
         bin,
         args=[],
         env=[]}).
-type exe() :: #exe{bin::string(),
                    args::[string()],
                    env::[{string(),string()}]}.

-record(master,
        {exe,
         supervisor,
         hosts,
         clients=ordsets:new() % all clients that used this pipe.
        }).

-type master() :: #master{exe::exe(),
                          hosts::[{node(),non_neg_integer()}],
                          supervisor::pid()
                         }.

-record(slave,
        {id, % unix pid
         pid, % erlang pid
         port,
         seq, % master's sequence number
         timeout=5000
        }).


%% message within pipe
-record(msg,
        {data, %% binary payload delivered to external process as UBF binary
         token, %% a token to identify the meesage
         handler %% where to send back result
        }).

-type msg() :: #msg{data::binary()}.

%% types of piperl messages at the interface
%% data message
-record(pipe_data,{data}).
%% meta messages
-record(pipe_ok,{}).
-record(pipe_call,{data,token}).
-record(pipe_sync,{token}).
-record(pipe_error,{reason,detail,data}).

