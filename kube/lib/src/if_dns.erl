%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(if_dns).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
%% --------------------------------------------------------------------



%% External exports
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


cast_info([{service,ServiceStr,_Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{sender_info,SenderInfo}])->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A},SenderInfo]),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:cast(DnsIp,DnsPort,{M,F,A},SenderInfo);
	      _->
		  Instances=tcp:call(DnsIp,DnsPort,{dns,get_instances,[ServiceStr]},1,SenderInfo),
		  case Instances of
		      []->
			  {error,[?MODULE,?LINE,'no service found',ServiceStr,SenderInfo]};
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err,SenderInfo]};
		      %->
		      [DnsInfo|_]->
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:cast(IpAddr,Port,{M,F,A},NumToSend,SenderInfo)
		  end
	  end,
    Reply.

cast([{service,ServiceStr,_Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend}])->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:cast(DnsIp,DnsPort,{M,F,A});
	      _->
		  Instances=tcp:call(DnsIp,DnsPort,{dns,get_instances,[ServiceStr]}),
		  case Instances of
		      []->
			  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err]};
		      %->
		      [DnsInfo|_]->
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:cast(IpAddr,Port,{M,F,A},NumToSend)
		  end
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

call_info([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_rec,NumToRec},{time_out,TimeOut},{sender_info,SenderInfo}])->
    % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Self=self(),
    ListOfPids=spawn_link(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,TimeOut,SenderInfo]),
    Instances=do_receive(ListOfPids,1,TimeOut,SenderInfo,[]),
    Reply=case Instances of
	      []->
		  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
	      {error,Err}->
		  {error,[?MODULE,?LINE,Err]};
	      Instances->
		  call_proc(Self,Instances,{M,F,A},NumToSend,NumToRec,TimeOut,SenderInfo) 	  
	  end,
    Reply.

call([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_tec,NumToRec},{time_out,TimeOut}])->
    Self=self(),
    ListOfPids=spawn_link(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,TimeOut,no_sender_info]),
    Instances=do_receive(ListOfPids,1,TimeOut,no_sender_info,[]),
    Reply=case Instances of
	      []->
		  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
	      {error,Err}->
		  {error,[?MODULE,?LINE,Err]};
	      Instances->
		  call_proc(Self,Instances,{M,F,A},NumToSend,NumToRec,TimeOut,no_sender_info)		  
	  end,
    Reply.
    

call_proc(Parent,Instances,{M,F,A},NumToSend,NumToRec,TimeOut,SenderInfo)->
    Diff=NumToSend-NumToRec,
    Result=case Diff<0 of
	       false->
		   ListOfPids=do_call(Parent,Instances,{M,F,A},NumToSend,TimeOut,SenderInfo,[]),
		   {ok,do_receive(ListOfPids,NumToRec,TimeOut,SenderInfo,[])};
	       true->
		  io:format("error ~p~n",[{?MODULE,?LINE,'more receive then send',{M,F,A},NumToSend,NumToRec,SenderInfo}]),
		  {error,[{?MODULE,?LINE,'more receive then send',{M,F,A},NumToSend,NumToRec,SenderInfo}]}
	   end, 
    Result.

do_receive(_,0,_,_,Result)->
    Result;
do_receive(ListOfPids,NumToRec,TimeOut,SenderInfo,Acc)->
    receive
	{_Pid,tcp_call_ack,Result}->
	    NewNumToRec= NumToRec-1,
	    NewAcc=[Result|Acc]
    after TimeOut+500 ->
	    NewAcc=[{error,[?MODULE,?LINE,tcp_timeout,SenderInfo]}|Acc],
	    NewNumToRec=0
    end,
    do_receive(ListOfPids,NewNumToRec,TimeOut,SenderInfo,NewAcc).
    
do_call(_,[],_,_,_,_,Pids)->
    Pids;
do_call(_,_,_,0,_,_,Pids)->
    Pids;
do_call(Self,[DnsInfo|T],{M,F,A},NumToSend,TimeOut,SenderInfo,Acc)->
    IpAddr=DnsInfo#dns_info.ip_addr,
    Port=DnsInfo#dns_info.port,    
    Pid=spawn_link(tcp,call,[IpAddr,Port,{M,F,A},Self,TimeOut,SenderInfo]),
    NewAcc=[Pid|Acc],
    do_call(Self,T,{M,F,A},NumToSend-1,TimeOut,SenderInfo,NewAcc).



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

