
all: flack

flack:
	$(MAKE) -C server/flack $@

test:
	./test

.PHONY: all test

