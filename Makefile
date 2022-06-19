.PHONY: default
default: all

# See also ‘HFLAGS_STATIC’ note.
DEPENDENCIES ?= \
	bytestring \
	template-haskell \
	#

.PHONY: show-dependencies
show-dependencies:
	@echo $(DEPENDENCIES)

HC ?= ghc
HFLAGS ?= $(BASE_HFLAGS) $(EXTRA_HFLAGS)
BASE_HFLAGS ?= $(CONFIG_HFLAGS) $(HFLAGS_QA) $(HFLAGS_OPTIMIZATION) $(HFLAGS_DEBUG)
EXTRA_HFLAGS ?=

HFLAGS_QA ?= -Wall
HFLAGS_OPTIMIZATION ?= -O2
HFLAGS_DEBUG ?=

CONFIG_HFLAGS ?= \
	-static -dumpdir $(DUMPDIR) -hidir $(HIDIR) -hiedir $(HIEDIR) \
	-odir $(ODIR) -outputdir $(OUTDIR) -stubdir $(STUBDIR) \
	#

# Optional flags for static linking.
#
# Note: for ‘-optl-static’, also ensure you have static libraries for gmp, ffi,
# and numa, e.g. ‘yay -S --needed libgmp-static libffi-static numactl-git’ on archlinux.
# (Thanks: https://ro-che.info/articles/2015-10-26-static-linking-ghc)
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--fwhole-archive-hs-libs
HFLAGS_STATIC ?= \
	-optl-static \
	-optl-pthread \
	-fwhole-archive-hs-libs \
	#
HFLAGS_STATIC_UNUSED ?= \
	-pgml ld.bfd \
	#

DUMPDIR ?= $(C_DIR)/dump
HIDIR ?= $(C_DIR)/hi
HIEDIR ?= $(C_DIR)/hie
ODIR ?= $(C_DIR)/odir
OUTDIR ?= $(C_DIR)/out
STUBDIR ?= $(C_DIR)/stub

# Rule spot, all-execs.

BUILD_DIR ?= ./build
DIST_DIR ?= $(BUILD_DIR)/$(DIST_BASENAME)
DIST_BASENAME ?= dist

DAEMON_EXEC ?= tollshray

SRC_DIR ?= ./src
EXEC_SRC ?= $(SRC_DIR)/$(EXEC_SRC_BASENAME)

EXEC_SRC_BASENAME ?= Tollshray.hs

C_DIR ?= $(BUILD_DIR)/hc

C_DIRS ?= \
	$(DUMPDIR) \
	$(HIDIR) \
	$(HIEDIR) \
	$(ODIR) \
	$(OUTDIR) \
	$(STUBDIR) \
	#

BUILD_DIRS ?= \
	$(BUILD_DIR) \
	$(DIST_DIR) \
	$(C_DIRS) \
	#

DIRS ?= \
	$(BUILD_DIRS) \
	$(SRC_DIR) \
	#

PREFIX ?= /usr
BIN_DIR ?= $(PREFIX)/$(BASE_BIN_DIR)

BASE_BIN_DIR ?= /bin

.PHONY: all
all: _build dist

# ‘_build’ for the phony rule to disambiguate the ‘build’ directory path.
.PHONY: _build
_build: build-dirs execs

.PHONY: build-dirs
build-dirs: $(BUILD_DIR) $(DIST_DIR)

.PHONY: execs
execs: $(BUILD_DIR)/$(DAEMON_EXEC)

$(BUILD_DIRS):
	mkdir -p "$@"

LIB_SRCS ?= \
	$(SRC_DIR)/Tollshray/Daemon/CLI.hs \
	#

$(BUILD_DIR)/$(DAEMON_EXEC): $(EXEC_SRC) $(LIB_SRCS) | $(BUILD_DIR)
	$(HC) $(HFLAGS) $(HFLAGS_STATIC) -o "$@" $^

.PHONY: dist
dist: execs | $(DIST_DIR)
	install -d -m 0775 -- "$(DIST_DIR)$(BIN_DIR)"
	install -m 0775 -- "$(BUILD_DIR)/$(DAEMON_EXEC)" "$(DIST_DIR)$(BIN_DIR)/$(DAEMON_EXEC)"

.PHONY: clean
clean:
	rm -rf -- "./build"
