## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-

REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

.PHONY: all compile deps clean test
.PHONY: release release_patch release_minor release_major
.PHONY: eunit xref dialyze

all: compile

compile:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) update-deps
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: eunit xref dialyze

#############################################################################
## release stuff

release_major: deps test
	./bin/release.sh major

release_minor: deps test
	./bin/release.sh minor

release_patch: deps test
	./bin/release.sh patch

release: release_patch

#############################################################################
## testing

eunit: compile
	@$(REBAR) eunit skip_deps=true

xref: compile
	@$(REBAR) xref skip_deps=true

dialyze: compile ~/.dialyzer_plt deps/.dialyzer_plt
	$(shell [ -d .eunit ] && rm -rf .eunit)
	dialyzer ebin -nn -Wno_return --plt deps/.dialyzer_plt

~/.dialyzer_plt:
	-dialyzer -nn --output_plt ${@} --build_plt \
           --apps erts kernel stdlib crypto ssl public_key inets \
                  eunit xmerl compiler runtime_tools mnesia syntax_tools

deps/.dialyzer_plt: ~/.dialyzer_plt
	-dialyzer -nn \
          --add_to_plt --plt ~/.dialyzer_plt --output_plt ${@} -r deps
