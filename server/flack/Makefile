
REBAR=GPROC_DIST=true ../../bin/rebar3

all: flack

flack: test release

test:
	${REBAR} eunit

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
