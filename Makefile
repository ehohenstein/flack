
all: test build release

MKDIR=mkdir -p
CP=cp

test:
	./test

build:
	$(MAKE) -C client/static $@
	$(MAKE) -C server/flack $@

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

.PHONY: all build test

