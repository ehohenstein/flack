
REBAR=GPROC_DIST=true ../../bin/rebar3

all: flack

flack: test release

test:
	${REBAR} eunit

release: compile
	${REBAR} release

tar: compile
	${REBAR} as prod tar

compile:
	${REBAR} compile

clean:
	rm -rf _build

start: release
	_build/default/rel/flack/bin/flack start

stop:
	_build/default/rel/flack/bin/flack stop

shell:
	${REBAR} shell
