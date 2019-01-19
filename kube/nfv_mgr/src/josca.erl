%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(josca).

% 
%% --------------------------------------------------------------------
%% Include files1
%% --------------------------------------------------------------------

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
%% 
info(FullFileName)->
    {ok,Info}=file:consult(FullFileName),
    {specification,NameStr}=lists:keyfind(specification,1,Info),
    {vsn,VsnStr}=lists:keyfind(vsn,1,Info),
    {NameStr,VsnStr,Info}.

files(Info)->
    R= case lists:keyfind(josca_files,1,Info) of
	   false->
	       [];
	   {josca_files,JoscaFiles}->
	       JoscaFiles
       end,
    R.

services(I)->
    R= case  lists:keyfind(services,1,I) of
	   false->
	       [];
	   {services,Service}->
	       Service
       end,
    R.

num_instances(I)->
    {num_instances,R}=lists:keyfind(num_instances,1,I),
    R.
zone(I)->
    R= case  lists:keyfind(zone,1,I) of
	   false->
	       [];
	   {zone,Zone}->
	       Zone
       end,
    R.

needed_capabilities(I)->
    R= case  lists:keyfind(needed_capabilities,1,I) of
	   false->
	       [];
	   {needed_capabilities,Capa}->
	       Capa
       end,
    R.

geo_red(I)->
    R= case  lists:keyfind(geo_red,1,I) of
	   false->
	       [];
	   {geo_red,GeoRed}->
	       GeoRed
       end,
    R.


%-----------------------------------------------------------------------
create(FullFileName)->
    {ok,Info}=file:consult(FullFileName),
    {specification,NameStr}=lists:keyfind(specification,1,Info),
    {vsn,VsnStr}=lists:keyfind(vsn,1,Info),
    if_dns:call("repo",[repo_mgr,store,[{josca,NameStr,VsnStr},Info]]).

x_delete(NameStr,VsnStr)->
     if_dns:call("repo",[repo_mgr,delete,[{josca,NameStr,VsnStr}]]).
	      
x_files(NameStr,VsnStr)->
    {ok,I}= if_dns:call("repo",[repo_mgr,get,[{josca,NameStr,VsnStr}]]),
    {josca_files,R}=lists:keyfind(josca_files,1,I),
    R.

x_services(NameStr,VsnStr)->
    {ok,I}= if_dns:call("repo",[repo_mgr,get,[{josca,NameStr,VsnStr}]]),
    {services,R}=lists:keyfind(services,1,I),
    R.

x_num_instances(NameStr,VsnStr)->
    {ok,I}= if_dns:call("repo",[repo_mgr,get,[{josca,NameStr,VsnStr}]]),
    {num_instances,R}=lists:keyfind(num_instances,1,I),
    R.
x_zone(NameStr,VsnStr)->
    {ok,I}= if_dns:call("repo",[repo_mgr,get,[{josca,NameStr,VsnStr}]]),
    {zone,R}=lists:keyfind(zone,1,I),
    R.

x_geo_red(NameStr,VsnStr)->
    {ok,I}= if_dns:call("repo",[repo_mgr,get,[{josca,NameStr,VsnStr}]]),
    {geo_red,R}=lists:keyfind(geo_red,1,I),
    R.



%% --------------------------------------------------------------------------


start_order(AppId,Vsn)->
    JoscaInfo=if_dns:call("repo",repo,read_josca_file,[AppId,Vsn]),
    Services=josca:services(JoscaInfo),
    JoscaFiles=josca:files(JoscaInfo),
%    io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
    Acc=case Services of 
	    []-> % Root is a Appfile
		[]; % is a service 
	    [{ServiceStr,VsnStr}]->
		[{{service,ServiceStr,VsnStr},{zone,josca:zone(JoscaInfo)},{needed_capabilities,josca:needed_capabilities(JoscaInfo)}}]
	end,
  %  io:format("~p~n",[{?MODULE,?LINE,Acc}]),
    Result=dfs(JoscaFiles,Acc),
    Result.


dfs([],Acc)->
    Acc;
dfs([{SpecStr,VsnJoscaStr}|T],Acc)->
 %   io:format("~p~n",[{?MODULE,?LINE,SpecStr,VsnJoscaStr}]),
    JoscaInfo=if_dns:call("repo",repo,read_josca_file,[SpecStr,VsnJoscaStr]),
  %  io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
    Acc1= case josca:services(JoscaInfo) of
	      []-> % App josca file
		  Acc;
	      [{ServiceStr,VsnStr}]->
		  [{{service,ServiceStr,VsnStr},{zone,josca:zone(JoscaInfo)},{needed_capabilities,josca:needed_capabilities(JoscaInfo)}}|Acc]
	  end,
   %  io:format("~p~n",[{?MODULE,?LINE,Acc1}]),
    JoscaFiles=josca:files(JoscaInfo),
  %  io:format("~p~n",[{?MODULE,?LINE,JoscaFiles}]),
    Acc2=dfs(JoscaFiles,Acc1),
    dfs(T,Acc2).  



%%-----------------------------------------------------------------------------


start_order_1(ServiceStr,VsnAppSpecStr)->
    {ok,AppSpecBaseName,AppSpecBinary}= if_dns:call("repo",[repo_mgr,get_app_spec_file,[ServiceStr,VsnAppSpecStr]]),    
    ok=file:write_file(AppSpecBaseName,AppSpecBinary),
    ServiceStr=app_spec:get_service_name(AppSpecBaseName),
    VsnStr=app_spec:get_service_vsn(AppSpecBaseName),
    Zone=app_spec:get_service_zone(AppSpecBaseName),
    BoardFunc=app_spec:get_service_board_functionality(AppSpecBaseName),
    NumInstances=app_spec:get_service_numInstances(AppSpecBaseName),
    GeoRed=app_spec:get_service_geo_red(AppSpecBaseName),
    NeededServices=app_spec:get_needed_services(AppSpecBaseName),
    Acc=[{ServiceStr,VsnStr,Zone,BoardFunc,NumInstances,GeoRed}],
    Result=dfs(NeededServices,Acc),   
    file:delete(AppSpecBaseName),
    Result.


dfs_1([],Acc)->
    Acc;
dfs_1([{ServiceStr,VsnAppSpecStr}|T],Acc)->
    {ok,_AppSpecBaseName,AppSpecBinary}= if_dns:call("repo",[repo_mgr,get_app_spec_file,[ServiceStr,VsnAppSpecStr]]), 
    TempFileName=rpc:call(node(),cmn,unique_id,[string])++".temp",
    ok=file:write_file(TempFileName,AppSpecBinary),
    ServiceStr=app_spec:get_service_name(TempFileName),
    VsnStr=app_spec:get_service_vsn(TempFileName),
    Zone=app_spec:get_service_zone(TempFileName),
    BoardFunc=app_spec:get_service_board_functionality(TempFileName),
    NumInstances=app_spec:get_service_numInstances(TempFileName),
    GeoRed=app_spec:get_service_geo_red(TempFileName),
    NeededServiceAppSpecs=app_spec:get_needed_services(TempFileName),

    file:delete(TempFileName),
    Acc1=[{ServiceStr,VsnStr,Zone,BoardFunc,NumInstances,GeoRed}|Acc],
    Acc2=dfs(NeededServiceAppSpecs,Acc1),
    dfs_1(T,Acc2).  
