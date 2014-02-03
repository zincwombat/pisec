# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
 
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
 
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

REBAR_CONFIG_X86=rebar.config.x86
RELEASE_CONFIG_X86=reltool.config.x86
REBAR_CONFIG_PI=rebar.config.pi
RELEASE_CONFIG_PI=reltool.config.pi
SYSCONFIG_PI=sys.config.pi
SYSCONFIG_X86=sys.config.x86
 
# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif
 
ARCHTYPE=$(shell uname -m)
$(info "archtype is $(ARCHTYPE)")

REL=rel

ifeq ($(ARCHTYPE),x86_64) 
$(info "(Intel x86 architecture detected) [$(ARCHTYPE)]")
REBAR_CONFIG=$(REBAR_CONFIG_X86)
RELEASE_CONFIG=$(RELEASE_CONFIG_X86)
SYS_CONFIG=$(SYSCONFIG_X86)
endif 

ifeq ($(ARCHTYPE),armv6l)
$(info "(Raspberry Pi (ARM) architecture detected) [$(ARCHTYPE)]")
REBAR_CONFIG=$(REBAR_CONFIG_PI)
RELEASE_CONFIG=$(RELEASE_CONFIG_PI)
SYS_CONFIG=$(SYSCONFIG_PI)
endif 

 
.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild setup
 
#all: deps compile dialyzer test
all: deps compile test

setup:
	rm -f  rebar.config 
	ln -s $(REBAR_CONFIG) rebar.config
	rm -f $(REL)/reltool.config
	ln -s $(RELEASE_CONFIG) $(REL)/reltool.config
	rm -f $(REL)/files/sys.config
	cp $(REL)/files/$(SYS_CONFIG) $(REL)/files/sys.config
 
# =============================================================================
# Rules to build the system
# =============================================================================

 
deps:	setup
	$(REBAR) get-deps
	$(REBAR) compile

release:	clean-deps 
	$(REBAR) compile
	$(REBAR) generate
 
update-deps:
	$(REBAR) update-deps
	$(REBAR) compile
 
compile:	setup
	$(REBAR) skip_deps=true compile
 
doc:
	$(REBAR) skip_deps=true doc
 
eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit
 
test: compile eunit
 
$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) -r deps
 
dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin
 
typer:
	typer --plt $(DEPS_PLT) -r ./src
 
shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)
 
pdf:
	pandoc README.md -o README.pdf
 
clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

clean-deps:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) clean
 
distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps
 
rebuild: distclean deps compile escript dialyzer test
