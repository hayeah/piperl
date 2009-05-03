
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
         processor % pid of slave's work queue
        }).

-record(slave_processor,
        {port,
         msg, %% message currently being processed
         timeout=5000 %% default timeout for slave. Could be overriden by msg timeout.
        }).

%% message within pipe
-record(msg,
        {data, %% binary payload delivered to external process as UBF binary
         handler, %% where to send back result
         timeout, %% overrides default slave timeout
         seq %% sequence number to uniquely identify a message for a client
        }).

-record(err_msg,
        {token,
         seq,
         reason, 
         retry=false %% piperl client would retry if this is true
        }).

-record(sync,{token,seq}).

-type msg() :: #msg{data::binary()}.

%%
