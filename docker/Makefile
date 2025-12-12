# SPDX-FileCopyrightText: 2025 Ivan Krasilnikov
# SPDX-License-Identifier: MIT

SHELL    := /bin/bash

DOCKER       := $(shell command -v podman 2>/dev/null || echo docker)
DOCKER_ARCH  := $(shell uname -m | sed -e 's/aarch64/arm64/; s/x86_64/amd64/')
DOCKER_BUILD := $(DOCKER) build --platform linux/$(DOCKER_ARCH)
DOCKER_RUN   := $(DOCKER) run --platform linux/$(DOCKER_ARCH)

IID_DIR  := ../.cache/iid
DIST_DIR := ../dist/$(DOCKER_ARCH)

FILE_TARGETS := $(filter-out base%, $(patsubst %.Dockerfile,%,$(wildcard [a-zA-Z0-9]*.Dockerfile)))
ARGS_TARGETS := $(shell cat args.txt | egrep -o '^[-a-zA-Z0-9_.]+' | sort -u)
TARGETS := $(sort $(FILE_TARGETS) $(ARGS_TARGETS))

all: base $(TARGETS)

base: $(IID_DIR)/base
	@true

$(IID_DIR)/base: base*.Dockerfile
	@mkdir -p $(IID_DIR)
	$(DOCKER_BUILD) -t jsz-debian -f base-debian.Dockerfile .
	$(DOCKER_BUILD) -t jsz-gcc14 -f base-gcc.Dockerfile --build-arg=VER=14 .
	$(DOCKER_BUILD) -t jsz-gcc15 -f base-gcc.Dockerfile --build-arg=VER=15 .
	$(DOCKER_BUILD) -t jsz-gcc16 -f base-gcc.Dockerfile --build-arg=VER=16 .
	$(DOCKER_BUILD) -t jsz-clang19 -f base-clang.Dockerfile --build-arg=VER=19 .
	$(DOCKER_BUILD) -t jsz-clang20 -f base-clang.Dockerfile --build-arg=VER=20 .
	$(DOCKER_BUILD) -t jsz-clang21 -f base-clang.Dockerfile --build-arg=VER=21 .
	$(DOCKER_BUILD) -t jsz-clang22 -f base-clang.Dockerfile --build-arg=VER=22 .
	$(DOCKER) image ls --format={{.Id}} jsz-debian >$(IID_DIR)/base

define rules
$(1): $(IID_DIR)/$(1) $(DIST_DIR)/$(1).json
	@true

$(IID_DIR)/$(1): $(IID_DIR)/base $$(shell ./build.sh --dep $(1)) build.sh
	./build.sh $(1)

$(DIST_DIR)/$(1).json: $(IID_DIR)/$(1) dist.sh
	./dist.sh $(1)
endef

$(foreach var,$(TARGETS),$(eval $(call rules,$(var))))

.PHONY:
