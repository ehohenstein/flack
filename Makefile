
all: test build release

MKDIR=mkdir -p
CP=cp

test:
	./test

build:
	$(MAKE) -C client/static $@
	$(MAKE) -C server/flack tar

release: build
	$(MAKE) -C server/flack $@

install:
	$(MKDIR) /var/www/
	$(MKDIR) /var/www/scripts/
	$(MKDIR) /var/www/css/
	$(MKDIR) /var/www/images/
	$(CP) client/static/flack-app.js /var/www/
	$(CP) client/static/flack.html /var/www/
	$(CP) client/static/scripts/jquery.min.js /var/www/scripts/
	$(CP) client/static/css/flack.css /var/www/css/
	$(CP) client/static/images/flack.png /var/www/images/

containers: build release
	$(MAKE) -C docker $@

docker-run-single: build
	$(MAKE) -C docker run-single

docker-stop-single:
	$(MAKE) -C docker stop-single

docker-run-cluster: build
	$(MAKE) -C docker run-cluster

docker-stop-cluster:
	$(MAKE) -C docker stop-cluster

clean:
	$(MAKE) -C client/static $@
	$(MAKE) -C server/flack $@

.PHONY: all build release test install containers docker-run-single docker-stop-single docker-run-cluster docker-stop-cluster clean

