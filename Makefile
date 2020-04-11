REBAR := rebar3

all:
	$(REBAR) do compile,edoc

test:
	$(REBAR) do eunit,ct,dialyzer,xref

clean:
	$(REBAR) clean

realclean:
	rm -rf _build

.PHONY: all test clean realclean
