
REBAR=../../bin/rebar3

all: flack

flack: release

release: compile
	${REBAR} release

compile:
	${REBAR} compile

clean:
	${REBAR} clean

start: release
	_build/default/rel/flack/bin/flack start

stop:
	_build/default/rel/flack/bin/flack stop

shell:
	${REBAR} shell
