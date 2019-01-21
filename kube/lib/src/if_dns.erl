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


cast_info([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{sender_info,SenderInfo}])->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A},SenderInfo]),
    Self=self(),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:cast(DnsIp,DnsPort,{M,F,A},NumToSend,SenderInfo);
	      _->
		  ListOfPids=spawn(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,10*1000,SenderInfo]),
		  Instances=do_receive(ListOfPids,1,10*1000,SenderInfo,[]),
		  case Instances of
		      [[]]->
			  io:format(" ~p~n",[{?MODULE,?LINE,'Error','no service found',ServiceStr}]),
			  {error,[?MODULE,?LINE,'no service found',ServiceStr,SenderInfo]};
		      {error,Err}->
			  io:format(" ~p~n",[{?MODULE,?LINE,'Error  ',Err,ServiceStr}]),
			  {error,[?MODULE,?LINE,Err,SenderInfo]};
		      %->
		      [[DnsInfo|_]]->			  
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:cast(IpAddr,Port,{M,F,A},NumToSend,SenderInfo),
			  {?MODULE,?LINE,IpAddr,Port,{M,F,A},NumToSend}
		  end
	  end,
    Reply.

cast([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend}])->
  %  io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Self=self(),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:cast(DnsIp,DnsPort,{M,F,A},NumToSend);
	      _->
		  ListOfPids=spawn(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,10*1000,no_sender_info]),
		  Instances=do_receive(ListOfPids,1,10*1000,no_sender_info,[]),
		%  io:format(" ~p~n",[{?MODULE,?LINE,Instances}]),
		  case Instances of
		      [[]]->
			  io:format(" ~p~n",[{?MODULE,?LINE,'Error','no service found',ServiceStr,Vsn}]),
			  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
		      {error,Err}->
			  io:format(" ~p~n",[{?MODULE,?LINE,'Error  ',Err,ServiceStr}]),
			  {error,[?MODULE,?LINE,Err]};
		      %->
		      [[DnsInfo|_]]->
	%		  io:format(" ~p~n",[{?MODULE,?LINE,DnsInfo}]),
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:cast(IpAddr,Port,{M,F,A},NumToSend),
			  {?MODULE,?LINE,IpAddr,Port,{M,F,A},NumToSend}
		  end
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

call_info([{service,ServiceStr,_Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_rec,NumToRec},{timeout,TimeOut},{sender_info,SenderInfo}])->
    % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Self=self(),
    ListOfPids=spawn(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr]},Self,TimeOut,SenderInfo]),
%    ListOfPids=spawn_link(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,TimeOut,SenderInfo]),
    ListOfResults=do_receive(ListOfPids,1,TimeOut,no_sender_info,[]),
    Reply=case ListOfResults of
	      [[]]->
		  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
	      [{error,Err}]->
		  {error,[?MODULE,?LINE,Err]};
	      ListOfResults->
		  [Instances]=ListOfResults,
	%	  io:format(" ~p~n",[{?MODULE,?LINE,Instances}]),
		  call_proc(Self,Instances,{M,F,A},NumToSend,NumToRec,TimeOut,SenderInfo) 	  
	  end,
    Reply.

call([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_rec,NumToRec},{timeout,TimeOut}])->
    Self=self(),
  %  ListOfPids=spawn_link(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr,Vsn]},Self,TimeOut,no_sender_info]),
    ListOfPids=spawn(tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr]},Self,TimeOut,no_sender_info]),
    ListOfResults=do_receive(ListOfPids,1,TimeOut,no_sender_info,[]),
    Reply=case ListOfResults of
	      [[]]->
		  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
	      [{error,Err}]->
		  {error,[?MODULE,?LINE,Err]};
	      ListOfResults->
		  [Instances]=ListOfResults,
	%	  io:format(" ~p~n",[{?MODULE,?LINE,Instances}]),
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
	{Pid,tcp_call_ack,Result}->
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
   %  io:format(" ~p~n",[{?MODULE,?LINE,DnsInfo}]),
    IpAddr=DnsInfo#dns_info.ip_addr,
    Port=DnsInfo#dns_info.port,    
%    Pid=spawn_link(tcp,call,[IpAddr,Port,{M,F,A},Self,TimeOut,SenderInfo]),
    Pid=spawn(tcp,call,[IpAddr,Port,{M,F,A},Self,TimeOut,SenderInfo]),
    NewAcc=[Pid|Acc],
    do_call(Self,T,{M,F,A},NumToSend-1,TimeOut,SenderInfo,NewAcc).



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

