SHELL=/bin/bash

#######
###
#   Variables for 'Curation'
###
#######

filepath-output := $(abspath ../bin)/
front-end-binary := science-broker-terminal
front-end-target := $(filepath-output)$(front-end-binary)

#######
###
#   Standard Targets
###
#######

.PHONY: all clean install installdirs

all:: $(front-end-target)

clean::
	-rm -f $(front-end-target)
	cabal clean
	cabal update

install:: $(front-end-target)

installdirs:: $(dir $(front-end-target))


PREREQS := $(shell command -v cabal 2> /dev/null)

#######
###
#   Build Targets
###
#######

$(front-end-target): $(dir $(filepath-target)) cabal.project science-broker.cabal app/* lib/*
ifndef PREREQS
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
	ghcup install ghc 9.4.5
	ghcup install cabal 3.8.1.0
endif
	cabal install exe:$(@F) \
	    --installdir=$(@D) \
	    --install-method=copy \
	    --overwrite-policy=always \
	    --enable-executable-stripping \
	    --enable-library-stripping \
	    --enable-static

$(dir $(front-end-target)):
	mkdir -p $@

