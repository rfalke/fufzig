REBAR=`which rebar || ./support/rebar`
all: deps compile
deps:
	@$(REBAR) get-deps
compile: deps
	@$(REBAR) compile
test: all
	@$(REBAR) skip_deps=true eunit
clean:
	rm -rf ebin */*~ *~ erl_crash.dump */erl_crash.dump src/*.beam
app: 
	@$(REBAR) clean
