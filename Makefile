DEPS_EXISTS=$(shell [ -d "deps" ] && echo 1 || echo 0)
UNAME_S=$(shell uname -s)


_help_:
	@echo make clean - clean project
	@echo make update - update all project deps
	@echo make release - build QRI release


clean:
	rebar clean

update:
ifeq ($(DEPS_EXISTS), 1)
	rebar update-deps
else
	rebar get-deps
endif
