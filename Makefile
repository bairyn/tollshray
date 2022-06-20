# The GNU Makefile for building the application executable.
#
# This is just one option available alongside Cabal (TODO).  If the
# dependencies are available in the environment, makefile should also be
# capable of using ‘ghc’ to build the application.  (TODO) Other build options
# are also available: stack, nix.

.PHONY: default
default: all

# See also ‘HFLAGS_STATIC’ note.
#
# e.g. ‘cabal update && cabal install …’
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
#
# # REMOVE pacman system Haskell packages, and install ghc and cabal-install
# manually:
#
# I didn't dig too deeply into this issue, but the root of it seems to be that
# the Archlinux ‘haskell-*’ packages are broken with user ‘cabal install blah’
# installations and builds.  So if you want to use ‘cabal-install’, clean up
# the user ‘~/.ghc’ and ‘~/.cabal’ packages by removing them, and clean up all
# Archlinux system pacman Haskell packages, ghc and cabal-install, ‘haskell-*’,
# and also ghc-libs, alex, happy, etc.
#
# Then build ‘ghc’ and ‘cabal-install’ by hand (you may temporarily need a
# system ghc, alex, and happy to bootstrap this; just look at what the build process
# tells you, and beware installing too much or this could also break the manual
# builds, and afterwards you can remove the temporary ‘ghc’, ‘happy’,
# ‘ghc-libs’, etc.  packages), and then you can have a consistent user
# installation and build your own Haskell packages with ‘cabal-install’ and
# such.
#
# Sorry for the hassle.  Maybe I should volunteer my time to fix ghc Haskell on
# Archlinux.
#
# *Sigh*, couldn't even build GHC.  Okay, I need a working Haskell installation
# before I can proceed on this project, unless I want to limit what libraries
# are available or I choose another language.
#
# OLD:
#
# If GHC can't find the files (BUILD ERROR), one solution may be to remove the
# package manager packages and dependencies with e.g. ‘haskell-utf8-string
# haskell-hledger-lib hledger darcs’ and to then install the user-wise package
# with e.g. ‘cabal install --lib utf8-string’ and ‘cabal install --lib
# language-rust’.  I had to repeat this pattern for many other packages also in
# order to get cabal-install to install language-rust under my user.
# (Archlinux: I removed all ‘haskell-*’ packages along with ghc and
# cabal-install, and also alex, happy, and ghc-libs, to get a clean setup.  I
# (eqv.) removed ~/.ghc and ~/.cabal.  Then I installed ghc and cabal-install,
# pacman packages.  Then I could finally build ‘language-rust’ with ‘cabal
# install language-rust’.  Wait, no, actually, I couldn't.)
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
