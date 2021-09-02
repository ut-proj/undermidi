PROJ = undermidi
PRIV = ./priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build:
	@rebar3 compile

rebuild: clean-all build

show-arch-bin:
	@echo $(DOWNLOAD_BIN)

check:
	-@rebar3 lfe clean
	@rebar3 lfe compile
	@rebar3 xref
	-@rebar3 dialyzer
	@rebar3 as test lfe ltest

recheck: rebuild check

clean-all: clean-go clean
	@rm -rf _build rebar.lock

.PHONY: default run build build-go clean-go clean-all

#############################################################################
###   Erlang Targets   ######################################################
#############################################################################

clean:
	@rebar3 clean

#############################################################################
###   Go Targets   ##########################################################
#############################################################################

GH_ORG = github.com/ut-proj
GO_PROJ = midiserver
GO_REPO = https://$(GH_ORG)/$(GO_PROJ).git
GO_BASE = $(PRIV)/go/src/$(GH_ORG)
GO_DIR = $(GO_BASE)/$(GO_PROJ)
GO_SRC = ~/lab/ut-proj/go/src/$(GH_ORG)/$(GO_PROJ)

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

rebuild-go: clean-go build-go