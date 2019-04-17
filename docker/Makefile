
containers:
	mkdir -p build
	cp start-flack.sh build/
	cp ../server/flack/_build/prod/rel/flack/flack-1.0.2.tar.gz build/
	cp ../client/static/flack.html build/
	cp ../client/static/flack-app.js build/
	mkdir -p build/scripts
	cp ../client/static/scripts/jquery.min.js build/scripts/
	mkdir -p build/css
	cp ../client/static/css/flack.css build/css/
	mkdir -p build/images
	cp ../client/static/images/flack.png build/images/
	docker build -t flack-server -f flack.Dockerfile build
	rm -rf build
	mkdir -p build
	cp nginx.conf build/
	docker build -t flack-balancer -f nginx.Dockerfile build
	rm -rf build

run-single: containers
	docker run -itd --name flack-single -p 8080:8080 flack-server

stop-single:
	docker stop flack-single
	docker rm flack-single

run-cluster: containers
	docker run -itd --hostname flack-svr1 --name flack-svr1 -e "FLACK_CLUSTER='flack@flack-svr1','flack@flack-svr2','flack@flack-svr3'" -p 8081:8080 flack-server
	docker run -itd --hostname flack-svr2 --link flack-svr1 --name flack-svr2 -e "FLACK_CLUSTER='flack@flack-svr1','flack@flack-svr2','flack@flack-svr3'" -p 8082:8080 flack-server
	docker run -itd --hostname flack-svr3 --link flack-svr1 --link flack-svr2 --name flack-svr3 -e "FLACK_CLUSTER='flack@flack-svr1','flack@flack-svr2','flack@flack-svr3'" -p 8083:8080 flack-server
	docker run -itd --link flack-svr1 --link flack-svr2 --link flack-svr3 --name flack-balancer -p 8080:8080 flack-balancer

stop-cluster:
	docker stop flack-svr1
	docker rm flack-svr1
	docker stop flack-svr2
	docker rm flack-svr2
	docker stop flack-svr3
	docker rm flack-svr3
	docker stop flack-balancer
	docker rm flack-balancer
