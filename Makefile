CCZE_EXISTS=$(shell [ -x "/usr/bin/ccze" ] && echo 1 || echo 0 )
DEPS_EXISTS=$(shell [ -d "deps" ] && echo 1 || echo 0)
UNAME_S=$(shell uname -s)
INSTALLER=apt-get install -y

ifeq ($(UNAME_S), Darwin)
	INSTALLER=brew install
endif


_help_:
	@echo make clean - clean project
	@echo make update - update all project deps


clean:
	rebar clean

update:
	@echo "==> Check ccze for proper displaing of the logs..."

ifeq ($(CCZE_EXISTS), 1)
	@echo "==> ok."
else
	@echo "==> Need to install ccze for proper displaing of the logs"
	sudo $(INSTALLER) ccze
endif

ifeq ($(DEPS_EXISTS), 1)
	rebar update-deps
else
	rebar get-deps
endif
