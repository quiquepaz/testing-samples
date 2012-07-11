.PHONY: deps test

all: deps compile

# Gets the dependencies to the deps folder. This is necessary for compile to succeed.
deps:
	rebar get-deps

# Compiles the whole application
compile:
	rebar compile
	@rebar skip_deps=true xref | grep -v "is unused export (Xref)"

# Cleans any generated files from the repo (except dependencies)
clean:
	rebar clean
	rm -f test/ebin/*
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

# Cleans any downloaded dependencies
distclean: clean
	rebar delete-deps

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
proper: compile
	@mkdir -p test/ebin
	@ERL_LIBS=$$ERL_LIBS:deps/ erlc test/ts_lists_proper.erl && mv ts_lists_proper.beam test/ebin
	erl -pa ebin -pa test/ebin -pa deps/*/ebin -eval "proper:module(ts_lists_proper)" -s init stop

proper_statem:compile
	@mkdir -p test/ebin
	@ERL_LIBS=$$ERL_LIBS:deps/ erlc test/resource_manager_proper_statem.erl && mv resource_manager_proper_statem.beam test/ebin
	erl -pa ebin -pa test/ebin -pa deps/*/ebin -eval "resource_manager:start_link([high, medium, low]), proper:module(resource_manager_proper_statem)" -s init stop

eunit: compile
	rebar skip_deps=true eunit

test: proper eunit

# Generates the edoc documentation and places it under doc/ .
docs:
	rebar skip_deps=true doc

# While developing with vi, :!make dialyzer | grep '%:t' can be used to run dialyzer in the current file
dialyzer: clean compile
	dialyzer -Wno_return -Wno_opaque -c ebin

typer: compile
	typer --show-exported -I include -I ../ src/*.erl

