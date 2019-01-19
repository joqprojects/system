-define(DNS_CALL_INFO,({ServiceStr,Vsn},{M,F,A},{DnsIp,DnsPort},NumToSend,NumToRec,TimeOut,SenderInfo),
       if_dns:call_info([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_tec,NumToRec},{time_out,TimeOut},{sender_info,SenderInfo}])).

-define(DNS_CALL,({ServiceStr,Vsn},{M,F,A},{DnsIp,DnsPort},NumToSend,NumToRec,TimeOut),
       if_dns:call([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_tec,NumToRec},{time_out,TimeOut}])).
-define(DNS_CAST,({ServiceStr,Vsn},{M,F,A},{DnsIp,DnsPort},NumToSend),
       if_dns:call([{service,ServiceStr,Vsn},{mfa,M,F,A},{dns,DnsIp,DnsPort},{num_to_send,NumToSend},{num_to_tec,0},{time_out,na}])).

-define(HEARTBEAT_INTERVAL,1*40*1000).
-define(INACITIVITY_TIMEOUT,1*125*1000).

% For testing 
-define (CONTROLLER_IP,"80.216.3.159").
-define (CONTROLLER_PORT,60012).
-define (ETCD_IP,"80.216.3.159").
-define (ETCD_PORT,60011).
-define (REPOSITORY_IP,"80.216.3.159").
-define (REPOSITORY_PORT,60011).
%-define(DNS_IP,"80.216.3.159").
%-define(DNS_PORT,60010).


-define (LOG_IP,"80.216.3.159").
-define (LOG_PORT,60011).


-define (W1_IP,"80.216.3.159").
-define (W1_PORT,50001).
-define (W2_IP,"80.216.3.159").
-define (W2_PORT,50002).
-define (W3_IP,"80.216.3.159").
-define (W3_PORT,50003).
-define (W4_IP,"80.216.3.159").
-define (W4_PORT,50004).

% Applications and services
-define (CALC_IP,"80.216.3.159").
-define (CALC_PORT,40001).
-define (ADDER_IP,"80.216.3.159").
-define (ADDER_PORT,40002).
-define (SUBTRACT_IP,"80.216.3.159").
-define (SUBTRACT_PORT,40003).
-define (DIVIDER_IP,"80.216.3.159").
-define (DIVIDER_PORT,40004).


