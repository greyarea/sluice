.PHONY: all depends compile run

all: compile

depends:
	./rebar get-deps
	./rebar update-deps

compile:
	./rebar compile

run:
	erl -pz deps/*/ebin ebin \
		-config priv/sys \
		-boot start_sasl \
		-s sluice
