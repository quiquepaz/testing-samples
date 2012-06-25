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

# Generates a release.
release: compile
	@mkdir -p /tmp/release_builder/
	@ln -s $(PWD)/. /tmp/release_builder/proper_samples
	unset ERL_LIBS; rebar generate force=1
	@rm -fr /tmp/release_builder/*

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
test: compile
	erl -pa ebin -pa deps/*/ebin -eval "proper:module(prs_basic)" -s init stop

# Generates the edoc documentation and places it under doc/ .
docs:
	rebar skip_deps=true doc

# Launches an erlang shell where the deps and the modules from the project are accesible
shell: compile
	rebar shell

# While developing with vi, :!make dialyzer | grep '%:t' can be used to run dialyzer in the current file
dialyzer: clean compile
	dialyzer -Wno_return -Wno_opaque -c ebin

typer: compile
	typer --show-exported -I include -I ../ src/*.erl

tag:
	@echo "Current version: $(TAG)" > DOC/CHANGELOG
	@git log --decorate  |\
         grep -E '(^ +(DOC|FIX|OPT|CHANGE|NEW|SEC|CHANGE|PERF))|tag:' |\
         sed 's/commit [0-9a-f]* (.*tag: \([0-9.]*\).*).*/\ntag: \1/'\
         >> DOC/CHANGELOG
	@git add DOC/CHANGELOG
	@git commit -m "--" DOC/CHANGELOG
	@git tag $(TAG)
