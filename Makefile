PROJ = undermidi
PRIV = ./priv
PWD = $(shell pwd)
ARCH = $(shell uname -m)
OS = $(shell uname -s|tr '[:upper:]' '[:lower:]')
ifeq ($(OS),linux)
  ifeq ($(ARCH),x86_64)
    ARCH = amd64
  endif
endif
MIDISERVER_VSN = 0.1.0
MIDISERVER_REPO = https://github.com/ut-proj/midiserver
BIN = midiserver
DOWNLOAD_BIN = $(BIN)-$(OS)-$(ARCH)
DOWNLOAD_LINK = $(MIDISERVER_REPO)/releases/download/$(MIDISERVER_VSN)/$(DOWNLOAD_BIN)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build:
	@rebar3 compile

rebuild: clean-all build

check:
	-@rebar3 lfe clean
	@rebar3 lfe compile
	@rebar3 xref
	-@rebar3 dialyzer
	@rebar3 as test lfe ltest

recheck: rebuild check

show-arch-bin:
	@echo $(DOWNLOAD_BIN)

clean-all: clean-go clean
	@rm -rf _build rebar.lock

.PHONY: default run build build-go clean-go clean-all download 

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
	git clone $(GO_REPO)

build-go: | $(GO_DIR)
	@echo ">> Building Go examples ..."
	@cd $(GO_DIR) && $(MAKE)

clean-go:
	@cd $(GO_DIR) && $(MAKE) clean

rebuild-go: clean-go build-go

download: | bin/$(BIN)

bin/$(BIN):
	@mkdir -p ./bin
	@echo ">> Downloading $(DOWNLOAD_LINK) ..."
	@curl -L --silent -O $(DOWNLOAD_LINK)
	@chmod 755 $(DOWNLOAD_BIN)
	@echo ">> Moving $(DOWNLOAD_BIN) to ./bin/$(BIN) ..."
	@mv $(DOWNLOAD_BIN) ./bin/$(BIN)

