## set the paths for a default setup
system:
	rm -rf test_ebin/* test_src/*~;
	erlc -o test_ebin test_src/*.erl;
	erl -pa test_ebin -pa ebin/lib/ebin -s test_system test -sname test_system;
clean:
	rm -rf ebin/*/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test_kube/*/kubelet_ebin/* test_kube/*/service_ebin/* test_kube/*/lib_ebin/*  ;
build:
	rm -rf ebin/*/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test_kube/*/kubelet_ebin/* test_kube/*/service_ebin/* test_kube/*/lib_ebin/*  ;
#lib
	erlc -o ebin/lib kube/lib/src/*.erl;
	cp kube/lib/src/*.app ebin/lib;
	cp kube/lib/src/*.josca ebin/lib;
# dns
	erlc -o ebin/dns kube/dns/src/*.erl;
	cp kube/dns/src/*.app ebin/dns;
	cp kube/dns/src/*.josca ebin/dns;
# log
	erlc -o ebin/log kube/log/src/*.erl;
	cp kube/log/src/*.app ebin/log;
	cp kube/log/src/*.josca ebin/log;
# controller
	erlc -o ebin/controller kube/controller/src/*.erl;
	erlc -o test_ebin kube/controller/test_src/*.erl;
	cp kube/controller/src/*.app ebin/controller;
	cp kube/controller/src/*.josca ebin/controller;
#etcd
	erlc -o ebin/etcd kube/etcd/src/*.erl;
	cp kube/etcd/src/*.app ebin/etcd;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/etcd/src/* ebin/etcd;
#catalog
	erlc -o ebin/catalog kube/catalog/src/*.erl;
	cp kube/catalog/src/*.app ebin/catalog;
	cp kube/catalog/src/* ebin/catalog;
#repository
	erlc -o ebin/repository kube/repository/src/*.erl;
	cp kube/repository/src/*.app ebin/repository;
	cp kube/repository/src/* ebin/repository;
#kubelet
	erlc -o ebin/kubelet kube/kubelet/src/*.erl;
	cp kube/kubelet/src/*.app ebin/kubelet;
	cp kube/kubelet/src/* ebin/kubelet;
# Services for ebin
#adder_100
	erlc -o ebin/adder_100 services/adder_100/src/*.erl;
	cp services/adder_100/src/*.app ebin/adder_100;
	cp services/adder_100/src/*.josca ebin/adder_100;
#subtract_100
	erlc -o ebin/subtract_100 services/subtract_100/src/*.erl;
	cp services/subtract_100/src/*.app ebin/subtract_100;
	cp services/subtract_100/src/*.josca ebin/subtract_100;
#divider_100
	erlc -o ebin/divider_100 services/divider_100/src/*.erl;
	cp services/divider_100/src/*.app ebin/divider_100;
	cp services/divider_100/src/*.josca ebin/divider_100;
#multi_100
	erlc -o ebin/multi_100 services/multi_100/src/*.erl;
	cp services/multi_100/src/*.app ebin/multi_100;
	cp services/multi_100/src/*.josca ebin/multi_100;
#
#	erlc -o ebin/calc/ebin services/calc/src/*.erl;
#	cp services/calc/src/*.app ebin/calc/ebin;
#	cp services/calc/src/*.josca ebin/calc/ebin;
#	cp services/calc/src/* ebin/calc/ebin;
#	cp ebin/common/ebin/* ebin/calc/ebin;
# Workers for test
#Controller
	cp ebin/kubelet/* test_kube/controller/kubelet_ebin;
	cp ebin/controller/* test_kube/controller/service_ebin;
	cp ebin/lib/* test_kube/controller/lib_ebin;
#etcd
	cp ebin/kubelet/* test_kube/etcd/kubelet_ebin;
	cp ebin/etcd/* test_kube/etcd/service_ebin;
#repository
	cp ebin/kubelet/* test_kube/repository/kubelet_ebin;
	cp ebin/lib/* test_kube/repository/lib_ebin;
	cp ebin/repository/* test_kube/repository/service_ebin;
#catalog
	cp ebin/kubelet/* test_kube/catalog/kubelet_ebin;
	cp ebin/lib/* test_kube/catalog/lib_ebin;
	cp ebin/catalog/* test_kube/catalog/service_ebin;
#dns
	cp ebin/kubelet/* test_kube/dns/kubelet_ebin;
	cp ebin/lib/* test_kube/dns/lib_ebin;
	cp ebin/dns/* test_kube/dns/service_ebin;
#workers
	cp ebin/kubelet/* test_kube/w1/kubelet_ebin;
	cp ebin/lib/* test_kube/w1/lib_ebin;
	cp ebin/kubelet/* test_kube/w2/kubelet_ebin;
	cp ebin/lib/* test_kube/w2/lib_ebin;
	cp ebin/kubelet/* test_kube/w3/kubelet_ebin;
	cp ebin/lib/* test_kube/w3/lib_ebin;
	cp ebin/kubelet/* test_kube/w4/kubelet_ebin;
	cp ebin/lib/* test_kube/w4/lib_ebin;
# master worker
#	cp test/w1/node/ebin/* test/w_master/node/ebin;
#	cp ebin/dns/ebin/* test/w_master/node/ebin;
#	cp ebin/repo/ebin/* test/w_master/node/ebin;
#	cp ebin/vim/ebin/* test/w_master/node/ebin;
#	cp ebin/nfv_mgr/ebin/* test/w_master/node/ebin;
#	cp ebin/log/ebin/* test/w_master/node/ebin;
	echo ++  build_succeded +++++++++++
start_etcd:
	erl -pa ebin/etcd/ebin -s etcd_lib boot -sname etcd
test_oam:
	cp ebin/common/ebin/* test/test_ebin;
	erl -pa test/test_ebin -sname test_oam
dns_test:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/dns/ebin kube/dns/src/*.erl;
	erlc -o test/test_ebin kube/dns/test_src/*.erl;
	cp kube/dns/src/*.app ebin/dns/ebin;
	cp ebin/common/ebin/* ebin/dns/ebin;
	erl -pa test/test_ebin -pa ebin/dns/ebin -s test_dns test -sname test_dns -setcookie glurk
kubelet_test:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/kubelet/ebin kube/kubelet/src/*.erl;
	erlc -o test/test_ebin kube/kubelet/test_src/*.erl;
	cp kube/kubelet/src/*.app ebin/kubelet/ebin;
	cp ebin/common/ebin/* ebin/kubelet/ebin;
	erl -pa test/test_ebin -pa ebin/kubelet/ebin -s test_kubelet test -sname test_kubelet -setcookie glurk
adder_test:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/adder/ebin services/adder/src/*.erl;
	erlc -o test/test_ebin services/adder/test_src/*.erl;
	cp services/adder/src/*.app ebin/adder/ebin;
	cp ebin/common/ebin/* ebin/adder/ebin;
	erl -pa test/test_ebin -pa ebin/adder/ebin -s test_adder test -sname test_adder -setcookie glurk
