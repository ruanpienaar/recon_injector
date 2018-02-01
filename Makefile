.PHONY: escriptize compile test clean rebar3

escriptize: compile
	@./rebar3 escriptize
	@cp _build/default/bin/recon_injector .

compile: rebar3
	@./rebar3 compile

clean:
	@./rebar3 clean

rebar3:
	@ls rebar3 || wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3