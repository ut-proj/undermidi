PROJ = undermidi
PRIV = ./priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build: build-go
	@rebar3 compile

rebuild: clean-all build

clean-all: clean-go clean

.PHONY: default run build build-go clean-go clean-all

#############################################################################
###   Erlang Targets   ######################################################
#############################################################################

clean:
	@rebar3 clean

#############################################################################
###   Go Targets   ##########################################################
#############################################################################

GH_ORG = github.com/geomyidia
GO_PROJ = erl-midi-server
GO_REPO = https://$(GH_ORG)/$(GO_PROJ).git
GO_BASE = $(PRIV)/go/src/$(GH_ORG)
GO_DIR = $(GO_BASE)/$(GO_PROJ)
GO_SRC = ~/lab/geomyidia/src/$(GH_ORG)/$(GO_PROJ)

$(GO_DIR):
	@mkdir -p $(GO_BASE)
	@cd $(GO_BASE) && \
	ln -s $(GO_SRC) . && \
	touch go

build-go: | $(GO_DIR)
	@echo ">> Building Go examples ..."
	@cd $(GO_DIR) && $(MAKE)

clean-go:
	@cd $(GO_DIR) && $(MAKE) clean
