.PHONY: all depends compile run

all: compile

depends:
	rebar get-deps
	rebar update-deps

compile:
	rebar compile skip_deps=true app=sluice

run:
	erl -pz deps/*/ebin ebin \
		-boot start_sasl \
		-s sluice
