# =============================================================================
# Define some variables to access 2 different source files depending on the
# architecture
# =============================================================================

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

 
.PHONY: all compile clean shell update-deps setup deps release
 
all: 		deps compile 

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

 
deps:		setup
		$(REBAR) get-deps
		$(REBAR) compile

release:	setup
		$(REBAR) clean
		$(REBAR) compile
		$(REBAR) generate
 
update-deps:	setup
		$(REBAR) update-deps
		$(REBAR) compile
 
compile:	setup
		$(REBAR) skip_deps=true compile
 
 
clean:
		$(REBAR) skip_deps=true clean

clean-deps:
		$(REBAR) clean
