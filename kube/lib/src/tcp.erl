%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(tcp).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-include("kube/include/trace_debug.hrl").
-include("kube/include/tcp.hrl").
-include("certificate/cert.hrl").

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-compile([export_all]).
%-export([call/4,call/3,cast/3,server_seq/1,server_parallel/1,par_connect/1]).


%%
%% API Function
%%
% call(RecIpAddr,RecPort,{M,F,A},{SenderIpAddr,SenderPort,SenderModule,SenderLine})->

cast(IpAddr,Port,{os,cmd,A},NumToSend,SenderInfo)->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],NumToSend,SenderInfo);

cast(IpAddr,Port,{M,F,A},NumToSend,SenderInfo)->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG],NumToSend,SenderInfo).


cast(IpAddr,Port,{os,cmd,A},NumToSend)->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],NumToSend,no_sender_info);

cast(IpAddr,Port,{M,F,A},NumToSend)->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG],NumToSend,no_sender_info).

send_cast(Addr,Port,Msg,NumToSend,SenderInfo)->
    Result= case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
		{ok,Socket}->
	  %  io:format("ok Socket  ~p~n",[{?MODULE,?LINE,Addr,Port,Msg,inet:socknames(Socket)}]),
		    gen_tcp:send(Socket,term_to_binary(Msg));
		{error,Err} ->
		    io:format("cast error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg,SenderInfo}]),
		    {error,[{?MODULE,?LINE,Err,Addr,Port,Msg,SenderInfo}]}
    end,	
    Result.

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------

call(IpAddr,Port,{os,cmd,A},CallerPid,TimeOut,SenderInfo)->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],CallerPid,TimeOut,SenderInfo);

call(IpAddr,Port,{M,F,A},CallerPid,TimeOut,SenderInfo)->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG],CallerPid,TimeOut,SenderInfo).

send_call(Addr,Port,Msg,CallerPid,TimeOut,SenderInfo)->
    case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	  %  io:format("ok Socket  ~p~n",[{?MODULE,?LINE,Addr,Port,Msg,inet:socknames(Socket)}]),
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=binary_to_term(Bin),
		    %io:format("send Result ~p~n",[{?MODULE,?LINE,Result,inet:socknames(Socket)}]),	    
		    gen_tcp:close(Socket);
		{error,Err} ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,Err,SenderInfo]},
		    gen_tcp:close(Socket)
	    after TimeOut ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,time_out,Addr,Port,Msg,SenderInfo}]),
		    Result={error,[?MODULE,?LINE,tcp_timeout,Addr,Port,Msg,SenderInfo]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err} ->
	    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg,SenderInfo}]),
	    Result={error,{Err,?MODULE,?LINE}}
    end,	
    CallerPid!{self(),tcp_call_ack,Result}.

%%------------------------------------------------------------------------------------------------    
    

call(IpAddr,Port,{os,cmd,A},CallerPid,TimeOut)->
    send_call(IpAddr,Port,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],CallerPid,TimeOut);

call(IpAddr,Port,{M,F,A},CallerPid,TimeOut)->
    send_call(IpAddr,Port,[{M,F,A},?KEY_MSG],CallerPid,TimeOut).

send_call(Addr,Port,Msg,CallerPid,TimeOut)->
    case gen_tcp:connect(Addr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	  %  io:format("ok Socket  ~p~n",[{?MODULE,?LINE,Addr,Port,Msg,inet:socknames(Socket)}]),
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=binary_to_term(Bin),
		    %io:format("send Result ~p~n",[{?MODULE,?LINE,Result,inet:socknames(Socket)}]),	    
		    gen_tcp:close(Socket);
		{error,Err} ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,Err]},
		    gen_tcp:close(Socket)
	    after TimeOut ->
		    io:format("send error ~p~n",[{?MODULE,?LINE,time_out,Addr,Port,Msg}]),
		    Result={error,[?MODULE,?LINE,tcp_timeout,Addr,Port,Msg]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err} ->
	    io:format("send error ~p~n",[{?MODULE,?LINE,Err,Addr,Port,Msg}]),
	    Result={error,{Err,?MODULE,?LINE}}
    end,	
    CallerPid!{self(),tcp_call_ack,Result}.


% Receive part
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_seq(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),  
    seq_loop(LSock).

seq_loop(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    single(Socket),
    seq_loop(LSock).

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_parallel(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),
    spawn(fun()-> par_connect(LSock) end),
    receive
	wait_for_ever->
	    ok
    end.

par_connect(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    spawn(fun()-> par_connect(LSock) end),
    single(Socket).
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
single(Socket)->
    receive
	{tcp, Socket, RawData}->
	    case binary_to_term(RawData) of
		[{M,F,A},?KEY_MSG]->
		    Reply=rpc:call(node(),M,F,A),
		    gen_tcp:send(Socket,term_to_binary(Reply));
		[{call,{M,F,A}},?KEY_MSG]->
		    Reply=rpc:call(node(),M,F,A),
		    gen_tcp:send(Socket,term_to_binary(Reply));
		[{cast,{M,F,A}},?KEY_MSG]->
		    io:format(" ~p~n",[{?MODULE,?LINE,{cast,{M,F,A}}}]),
		    A=rpc:cast(node(),M,F,A),
		    io:format("Error ~p~n",[{?MODULE,?LINE,A}]);
		Err->
		    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
	    end;
	{tcp_closed,Socket} ->
	    exit
    end.
