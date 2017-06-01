DEPS_EXISTS=$(shell [ -d "deps" ] && echo 1 || echo 0)
UNAME_S=$(shell uname -s)


_help_:
	@echo make clean - clean project
	@echo make update - update all project deps


clean:
	rebar clean

update:
ifeq ($(DEPS_EXISTS), 1)
	rebar update-deps
else
	rebar get-deps
endif


SERVICE_TAG = $(service)
SERVICE_HOME = $(CURDIR)
SERVICE_TEMPLATE = $(CURDIR)/templates/$(SERVICE_TAG).service
TARGET_FILENAME = orderry.$(SERVICE_TAG).service
DESTINATION = /etc/systemd/system
DESTINATION_FILE = $(DESTINATION)/$(TARGET_FILENAME)

install.systemd: ## Install systemd service from template: sudo make install.systemd service=<name>
	@echo "Installing [$(SERVICE_TAG)] as systemd service ..."
	@echo "Home dir: $(SERVICE_HOME)"
	@echo "Template: $(SERVICE_TEMPLATE)"
	@echo "=> $(DESTINATION_FILE)"
	@rm -f $(DESTINATION_FILE)
	@sed 's|{{ HOME }}|$(SERVICE_HOME)|g' <$(SERVICE_TEMPLATE) >$(DESTINATION_FILE)
	@systemctl daemon-reload
	@echo "Finished"
