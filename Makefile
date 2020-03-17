REBAR := rebar3

all:
	$(REBAR) compile

test:
	$(REBAR) do eunit,ct,dialyzer,xref

clean:
	$(REBAR) clean

realclean:
	rm -rf _build

.PHONY: all test clean realclean
