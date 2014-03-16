.PHONY : all release fast-release test dialyze xref doc clean distclean
APPLICATION=erod
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling -Wrace_conditions

all: rebar
	@./rebar update-deps
	@rm -rf ./ebin || exit 0
	@./rebar compile

#release: xref dialyze test fast-release
release: xref dialyze fast-release

fast-release: all
	@rm -rf rel/$(APPLICATION) || exit 0
	@./rebar generate

test: all
	@./rebar ct skip_deps=true

dialyze: all ../dialyzer.plt
	@dialyzer --plt ../dialyzer.plt $(DIALYZER_WARNINGS) --src \
	          -I deps -I apps \
	          deps/cowboy/src deps/cowlib/src \
	          deps/ranch/src deps/jsx/src \
	          apps/erodlib/src apps/erod/src \
	          apps/erodws/src apps/erdom/src || exit 0

../dialyzer.plt:
	dialyzer --build_plt --output_plt ../dialyzer.plt \
           --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools runtime_tools

xref: all
	@./rebar xref skip_deps=true

doc: all
	@./rebar doc skip_deps=true

clean:
	@(test -f rebar && ./rebar clean) || exit 0
	@rm -rf rel/$(APPLICATION) || exit 0

distclean: clean
	@rm -f rebar || exit 0
	@rm -rf doc || exit 0
# Do not removes deps that are links	
	@find deps -type d '!' -name deps -prune -exec rm -rf {} ';' || exit 0

rebar: deps/rebar
	@(cd deps/rebar; escript bootstrap; mv rebar ../..)

deps/rebar:
	@mkdir -p deps 
	@git clone https://github.com/rebar/rebar.git deps/rebar
