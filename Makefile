# The GNU Makefile for building the application executable.
#
# This is just one option available alongside Cabal (TODO).  If the
# dependencies are available in the environment, makefile should also be
# capable of using ‘ghc’ to build the application.  (TODO) Other build options
# are also available: stack, nix.
#
# Note: beware mixing global system Haskell packages with user cabal-install
# packages.  On my Archlinux system, it's completely broken; probably I can
# only do one or the other.  For now, I'm just trying isolated environments
# with nix and stack.

.PHONY: default
default: all

# See also ‘HFLAGS_STATIC’ note.
#
# Show the libraries providing the modules that GHC needs in order to build
# this application.
#
# e.g. ‘cabal update && cabal install …’, but unfortunately currently Archlinux
# system packages seem broken with cabal-install user packages.  Perhaps I need
# to volunteer my time to fix it.  If I can find the time, while I'm at it, and
# I may as well work on getting ‘ghc’ more easily bootstrappable, since on
# Archlinux the system packages for ‘ghc’ were broken; this is a second project
# I could volunteer for.
#
# Otherwise mixing the two is bugged and results in BUILD ERRORs like ‘GHC
# can't find the files’, e.g.:
#
# Note on cabal-install BUILD ERRORs if GHC can't find the files, e.g.
# 	
# 	[2 of 7] Compiling Network.Socks5.Types ( Network/Socks5/Types.hs, dist/build/Network/Socks5/Types.o, dist/build/Network/Socks5/Types.dyn_o )
# 	
# 	Network/Socks5/Types.hs:24:1: error:
# 	    Could not find module ‘Network.Socket’
# 	    There are files missing in the ‘network-3.1.2.7’ package,
# 	    try running 'ghc-pkg check'.
# 	    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
# 	   |
# 	24 | import Network.Socket (HostAddress, HostAddress6, PortNumber)
# 	   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 	cabal: Failed to build socks-0.6.1 (which is required by http-conduit-2.3.8).
# 	See the build log above for details.
DEPENDENCIES ?= \
	bytestring \
	template-haskell \
	language-rust \
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
