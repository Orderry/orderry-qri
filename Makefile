
# Self-Documented Makefile approach, borrowed from: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-.]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'


cleanall: clean ## Clean project and rebar
	@rm -rf rebar


clean:  ## Clean project
	@rm -rf .rebar
	@rm -rf deps
	@rm -rf ebin
	@rm -rf .erlang.cookie
	@rm -rf erl_crash.dump


setup: cleanall ## Make build/runtime environment
	@echo "Making build/runtime environment..."
	@echo "Making Python environment..."
	@pipenv sync --bare --dev
	# Temporal pip downgrade for Pipenv compatibility
	@pipenv run pip install pip==9.0.1
	@pipenv check
	@echo "Installing rebar..."
	git clone git://github.com/rebar/rebar.git
	cd rebar && ./bootstrap


build: clean  ## Build project
	./rebar/rebar get-deps
	#./rebar/rebar update-deps
	./rebar/rebar compile


run:  ## Start QRI service in foreground with dev.config
	erl -sname orderry-qri -pa ebin deps/*/ebin -s qri_app -config dev +K true
