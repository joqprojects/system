%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_adder).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/tcp.hrl").
-include("../include/dns.hrl").
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
%test()->
 %   R1=rpc:call(node(),test_adder,start_test,[],5000),
  %  io:format("~p~n",[{?MODULE,?LINE,R1}]),
   % R2=rpc:call(node(),test_adder,stop_test,[],5000),
   % io:format("~p~n",[{?MODULE,?LINE,R2}]),
   % ok.
%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------



start_test()->
    adder:app_start(?ADDER_IP,?ADDER_PORT,"adder","1.0.0"),
    ok.
%% Build and release a service and application josca
calc_1_test()->
    42=adder:add(20,22),
    ok.

crash_test()->
    adder:crash(),
    ok.

calc_2_test()->
    42=adder:add(20,22),
    ok.

stop_test()->    
    cmn:stop_node('adder@joq-desktop'),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
