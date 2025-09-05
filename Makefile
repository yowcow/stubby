REBAR := rebar3

all:
	$(REBAR) do compile,edoc

docs:
	$(REBAR) edoc

test:
	$(REBAR) do eunit,ct,dialyzer,xref

clean:
	$(REBAR) clean

realclean:
	rm -rf _build

format:
	$(REBAR) efmt -w -- rebar.config src/** test/**

.PHONY: all docs test clean realclean format
