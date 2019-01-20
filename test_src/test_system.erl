%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_system).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
%-include_lib("eunit/include/eunit.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
%% --------------------------------------------------------------------
-compile(export_all).
%-export([test]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
add(AppId)->
    if_dns:call([{service,"controller","1.0.0"},{mfa,controller,add,[AppId,"1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
    ok.

remove(AppId)->
     if_dns:call([{service,"controller","1.0.0"},{mfa,controller,remove,[AppId,"1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
    ok.

stop_test()->
    if_dns:call("controller",controller,remove,["app_adder","1.0.0"]),
    if_dns:call("controller",controller,remove,["mymath","1.0.0"]),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
