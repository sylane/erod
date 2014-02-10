.PHONY : all release fast-release test dialyze xref doc clean distclean
APPLICATION=erod
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling -Wrace_conditions

all: rebar
	@./rebar get-deps
	@./rebar compile

#release: xref dialyze test fast-release
release: fast-release

fast-release: all
	@rm -rf rel/$(APPLICATION) || exit 0
	@rebar generate

test: all
	@./rebar ct skip_deps=true

dialyze: all ../dialyzer.plt
	@dialyzer --plt ../dialyzer.plt $(DIALYZER_WARNINGS) --src \
	          apps/erod/src || exit 0

../dialyzer.plt:
	dialyzer --build_plt --output_plt ../dialyzer.plt \
           --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools

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

rebar:
	@mkdir -p deps || exit 0 
	@git clone https://github.com/rebar/rebar.git deps/rebar
	@(cd deps/rebar; escript bootstrap; mv rebar ../..)
