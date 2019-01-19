%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(divider_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    {ok,PublicIp}=application:get_env(public_ip),
    {ok,PublicPort}=application:get_env(public_port),
    {ok,LocalIp}=application:get_env(local_ip),
    {ok,LocalPort}=application:get_env(local_port),
    {ok,Service}=application:get_env(service),
    {ok,Vsn}=application:get_env(vsn),
    InitArgs=addr_mgr:update_init_args(PublicIp,PublicPort,LocalIp,LocalPort,Service,Vsn),

    {ok,Pid}= divider_sup:start_link(InitArgs),
    {ok,Pid}.
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
