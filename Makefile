
# Self-Documented Makefile approach, borrowed from: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-.]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'


cleanall: clean  ## Clean project and rebar
	@rm -rf rebar


clean:  ## Clean project
	@rm -rf .rebar
	@rm -rf deps
	@rm -rf ebin
	@rm -rf .erlang.cookie
	@rm -rf erl_crash.dump


setup: cleanall  ## Make runtime environment
	@echo "Making build/runtime environment..."
	@echo "Making Python environment..."
	@pipenv sync --bare
	@pipenv check
	@echo "Installing rebar..."
	git clone git://github.com/rebar/rebar.git
	cd rebar && ./bootstrap


devsetup: cleanall  ## Make build/runtime environment for development
	@echo "Making build/runtime environment for development..."
	@echo "Making Python environment..."
	@pipenv sync --bare --dev
	@pipenv check
	@echo "Installing rebar..."
	git clone git://github.com/rebar/rebar.git
	cd rebar && ./bootstrap


codestyle:  ## Check code style
	@echo "Checking code style..."
	@pipenv run pycodestyle *.py --ignore=E501


build: clean  ## Build project
	./rebar/rebar get-deps
	#./rebar/rebar update-deps
	./rebar/rebar compile


run:  ## Start QRI service in foreground
	erl -sname orderry-qri -pa ebin deps/*/ebin -s qri_app +K true
