-module(server).
-export([init/1]). 
-export([dbManagerLoop/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simple Http server functionality.
%%% * handles incoming connections on pre-defined port
%%% * forwards received data to specified handlers
%%% * XXX Could add abstraction for handler parameters
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
run(Port,Handler, HandlerParams)->
    {ok, LSocket} = gen_tcp:listen(Port,?TCP_OPTIONS),
    io:format("Creating server on port ~p ~n", [Port]),
    io:fwrite("reqh dbm pid ~p ~n",[HandlerParams]),
    spawn(fun() -> accept(LSocket,Handler,HandlerParams) end).

accept(LSocket,Handler,HandlerParams)->
    {ok, Socket} = gen_tcp:accept(LSocket),
    HandlerPid = spawn(fun()-> Handler(Socket,HandlerParams) end),
    gen_tcp:controlling_process(Socket, HandlerPid),
    accept(LSocket,Handler,HandlerParams).

% Example handler that replies with received data.
%echoHandler(Socket)->
%    inet:setopts(Socket, [{active, once}]),
%    receive
%        {tcp, Socket, Data}->
%            io:format("Got packet: ~p~n", [Data]),
%            gen_tcp:send(Socket, Data),
%            echoHandler(Socket); 
%        {tcp_closed,Socket}->
%            io:format("Socket ~p closed~n", [Socket]);
%        {tcp_error, Socket, Reason} ->
%            io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
%    end,
%    io:format("Handler closed for socket ~p~n",[Socket]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application logic

getPid(PidManager)->
    PidManager ! {givePid,self()},
    receive
        {pid,Pid} -> Pid
    end.

% Request handler.
% * Parses request and forwards it to DB process
requestHandler(Socket,DBPidManager)->
    Error="404 ERROR\r\n",
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data}->
            DBPid=getPid(DBPidManager),
            io:format("Got packet: ~p, processing with db ~p ~n", [Data,DBPid]),
            Cmd=parser:parse(erlang:binary_to_list(Data)),
            case Cmd of
                [load_aggregated] ->
                    DBPid ! {{load_aggregated},self()},
                    receive     
                        {JSON,DBPid} ->                       
                            Ret = "199 OK\r\n"++JSON;
                        {_,DBPid} ->
                            Ret =Error
                    end; 
                [increment,U,C] ->
                    io:format("Handling command: ~p ~n", [Cmd]),
                    DBPid ! {{increment,U,C},self()},
                    receive
                        {incr_ok,DBPid} ->
                            Ret = "200 OK\r\n";
                        {_,DBPid} ->
                            Ret=Error
                    end;
                [_] ->
                    Ret = Error
                end,
            % TODO parsi error, saada 404
            % muidu saada 200 oK
            io:fwrite(Ret,[]),
            ok=gen_tcp:send(Socket, Ret),
            ok=gen_tcp:close(Socket),
            requestHandler(Socket,DBPidManager); 
        {tcp_closed,Socket}->
            io:format("Socket ~p closed~n", [Socket]);
        {tcp_error, Socket, Reason} ->
            io:format("Error on socket ~p reason: ~p~n", [Socket, Reason]);
        _ ->
            io:format("BLA: ~p~n", [f])
    end.

% Sets up database connection (Pid) and restarts database if necessary e.g. when database becomes full.
dbManagerLoop(MaxEntries)->
    P=urldb:init(MaxEntries),
    dbManagerLoop(P,MaxEntries).
dbManagerLoop(DBPid,MaxEntries)->
    %%% Add monitor for database process
    %erlang:monitor(process, DBPid),
    %io:fwrite("created monitor for, pid ~p ~n",[DBPid]),

    %%% Create link to database process and trap its exit signal
    link(DBPid),
    process_flag(trap_exit, true),
    receive
        {givePid,Pid} -> 
            io:fwrite("Sent db pid~n",[]),
            Pid ! { pid, DBPid},
            dbManagerLoop(DBPid,MaxEntries);
%         {'DOWN', Ref, process, DBPid, Reason} ->
%           io:fwrite("Database process down. Reason ~p, Ref=~p.~nRestarting DB...~n",[Reason,Ref]),
        {'EXIT',FromPid,Reason} ->
            io:fwrite("Database process exited. Reason ~p, Ref=~p.~nRestarting DB...~n",[Reason,FromPid]),
            dbManagerLoop(MaxEntries)
    end.

init(Port)->
    P=spawn(server, dbManagerLoop,[10]),
    run(Port,fun requestHandler/2,P).
