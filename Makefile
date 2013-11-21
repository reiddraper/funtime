PLT=$(CURDIR)/.plt
DEPS=erts kernel stdlib

.PHONY: test xref dialyzer rebuild

all: compile test qc xref dialyzer

compile:
	@./rebar compile

clean:
	@- rm -rf $(CURDIR)/ebin
	@./rebar clean

test: compile
	@./rebar skip_deps=true eunit

qc: compile
	@./rebar qc

xref: compile
	@./rebar xref

clean_plt:
	- rm $(PLT)

$(PLT):
	@echo Building local plt at $(PLT)
	@echo
	@dialyzer --output_plt $(PLT) --build_plt \
	   --apps $(DEPS)

dialyzer: $(PLT) compile
	@dialyzer --fullpath --plt $(PLT) \
		-Wunmatched_returns -Wno_return\
		-r ./ebin

rebuild: clean clean_plt all
