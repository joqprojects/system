%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/controller/src/controller_local.hrl").


-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/kubelet_data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
stop_services([],DnsList)->
  %  io:format("ok  ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    ok;
stop_services([{ServiceId,Vsn}|T],DnsList)->
 %   io:format("ServiceId,Vsn, Tail ~p~n",[{time(),?MODULE,?LINE,ServiceId,Vsn,'Tail',T}]),
 %   io:format("DnsList,Vsn ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    ListWithIp=[{IpAddr,Port,ServiceId,Vsn}||#dns_info{service_id=X_Id,
							      vsn=X_Vsn,
							      ip_addr=IpAddr,
							      port=Port}<-DnsList,
						    {ServiceId,Vsn}=:={X_Id,X_Vsn}],
  %  io:format("ListWithIp,Vsn ~p~n",[{time(),?MODULE,?LINE,ListWithIp}]),

   R= [{IpAddr,Port,X_ServiceId,X_Vsn,tcp:cast(IpAddr,Port,{kubelet,stop_service,[X_ServiceId]},1)}||{IpAddr,Port,X_ServiceId,X_Vsn}<-ListWithIp],
  %  io:format("result stop_service ~p~n",[{?MODULE,?LINE,R}]),
    stop_services(T,DnsList).
						  
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
needed_services(ApplicationList,State)->
    needed_services(ApplicationList,State,[]).

needed_services([],_,NeededServices)->
    NeededServices;
needed_services([{{AppId,Vsn},JoscaFile}|T],State,Acc)->
    {dependencies,ServiceList}=lists:keyfind(dependencies,1,JoscaFile),
    NewAcc=check_services(ServiceList,State,Acc),
    needed_services(T,State,NewAcc).

check_services([],_,Acc)->
    Acc;
check_services([{Id,Vsn}|T],State,Acc) ->
    NewAcc=case josca:start_order(Id,Vsn,State) of
	       {error,Err}->
		   io:format("error~p~n",[{?MODULE,?LINE,Err}]),
		   Acc;
	       Services ->
		   case lists:member({Id,Vsn},Acc) of
		       true->
			   Acc;
		       false->
			   lists:append(Services,Acc)
		   end
	   end,
    check_services(T,State,NewAcc).

missing_services(NeededServices,DnsList)->
    AvailibleServices=[{DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}||DnsInfo<-DnsList],
    [{Id,Vsn}||{Id,Vsn}<-NeededServices, 
	       lists:member({Id,Vsn},AvailibleServices)=:=false].


start_services([],Nodes,State)->
    ok;
start_services([{ServiceId,Vsn}|T],Nodes,State)->
    io:format("~p~n",[{?MODULE,?LINE,ServiceId,Vsn,Nodes}]),
    {dns,DnsIp,DnsPort}=State#state.dns_addr,
    R= if_dns:call([{service,"catalog",latest},{mfa,catalog,read,[ServiceId,Vsn]},
		    {dns,DnsIp,DnsPort},{num_to_send,1},{num_to_rec,1},{timeout,10*1000}]),

    io:format("~p~n",[{?MODULE,?LINE,R}]),
    case R of
	[{error,Err}]->
	    NewState=State,
	    {error,[?MODULE,?LINE,ServiceId,Vsn,Err]};
	{ok,[{ok,_,JoscaInfo}]}->
	 %	    io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
	    {zone,WantedZone}=lists:keyfind(zone,1,JoscaInfo),
	    {needed_capabilities,WantedCapabilities}=lists:keyfind(needed_capabilities,1,JoscaInfo),
	    NodesFullfilledNeeds=get_nodes_fullfills_needs(WantedZone,WantedCapabilities,Nodes),
	    io:format("~p~n",[{?MODULE,?LINE,ServiceId,WantedZone,WantedCapabilities,'=>>',NodesFullfilledNeeds}]),
	    case NodesFullfilledNeeds of
		[]->
		    io:format("~p~n",[{?MODULE,?LINE,'error no availible nodes'}]);
		NodesFullfilledNeeds->
		    io:format("~p~n",[{?MODULE,?LINE,NodesFullfilledNeeds }]),
		    R=schedule_start(ServiceId,Vsn,NodesFullfilledNeeds,State),
		    io:format("~p~n",[{?MODULE,?LINE,'Service start result =',R,ServiceId,Vsn}])
	    end;
	Err->
	    io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
	     {error,[?MODULE,?LINE,ServiceId,Vsn,Err]}
    end,
    start_services(T,Nodes,State).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
schedule_start(ServicesId,Vsn,NodesFullfilledNeeds,State)->
    [KubeleteInfo|_]=NodesFullfilledNeeds,
    io:format("~p~n",[{?MODULE,?LINE,KubeleteInfo }]),
    IpAddr=KubeleteInfo#kubelet_info.ip_addr,
    Port=KubeleteInfo#kubelet_info.port,
    Self=self(),
    io:format("~p~n",[{?MODULE,?LINE }]),
    Pid=spawn(tcp,call,[IpAddr,Port,{kubelet,start_service,[ServicesId,Vsn]},Self,4*1000,no_sender_info]),
    receive
	{Pid,tcp_call_ack,Result}->
	    ok
    after 4*1000 ->
	    Result={error,[?MODULE,?LINE,timeout,'start service  ',ServicesId,Vsn]}
    end,
    io:format("~p~n",[{?MODULE,?LINE,Result}]),
    Result.




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_nodes_fullfills_needs(WantedZone,WantedCapabilities,AvailibleNodes)->
    % Which nodes is in needed zone
    RightZone = case WantedZone of
		    []->
			AvailibleNodes;
		    Zone ->
		%	io:format("Zone=  ~p~n",[{?MODULE,?LINE,Zone}]), 
			[Node||Node<-AvailibleNodes,
				Node#kubelet_info.zone=:=Zone]
		end,
  %  io:format("RightZone  ~p~n",[{?MODULE,?LINE,RightZone}]),    
    NodesFullfilledNeeds=case WantedCapabilities of
			     []->
				 RightZone;
			     WantedCapabilities->
				 [Node||Node<-RightZone,
					check_capbility(WantedCapabilities,Node)]
			 end,
    
    NodesFullfilledNeeds.


check_capbility(WantedCapabilities,Node)->
    check_capbility(WantedCapabilities,Node,false).
    
check_capbility([],_,Boolean)->
    Boolean;
check_capbility([WCap|T],Node,_)->    
    case lists:member(WCap,Node#kubelet_info.capabilities) of
	false->
	    Tail=[],  % Stop searching
	    R=false;  % Failed
	true->
	    Tail=T,   % Continue search
	    R=true    % Succeded 
    end,
    check_capbility(Tail,Node,R).    
	   
				
    % Which nodes in needed zone has the right capabilities



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
