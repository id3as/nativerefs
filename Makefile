SHELL = bash
UNAME := $(shell uname)
VPATH = priv


#------------------------------------------------------------
# Find erlang install
space :=
space +=
s+ = $(subst $(space),+,$1)
+s = $(subst +,\$(space),$1)
ifeq ($(UNAME), MINGW64_NT-10.0)
  ERLDIR ?= $(call +s,$(addsuffix ../.., $(dir $(call s+,$(shell which erl)))))
else ifneq ($(_KERL_ACTIVE_DIR),)
  ERLDIR ?= $(_KERL_ACTIVE_DIR)
else
  ERLDIR ?= $(shell which erl | awk '{if ($$1 ~ /lib\/erlang\/erts/) { gsub("/lib/erlang/erts.*/bin/erl", "", $$1); print } else { gsub("/bin/erl", "", $$1); print }}')/lib/erlang
endif

#------------------------------------------------------------
# Default CFLAGS
DEFAULT_CFLAGS += -g -O3 -Wall -fPIC -I $(ERLDIR)/usr/include

#------------------------------------------------------------
# Default LDFLAGS
DEFAULT_LDFLAGS += -shared -fPIC -lpthread -lbz2 -L $(realpath .)/priv -L $(ERLDIR)/usr/lib -lei

#------------------------------------------------------------
# OS Overrides
ifeq ($(UNAME), MINGW64_NT-10.0)
  SO_EXT = dll
endif

ifeq ($(UNAME), Darwin)
  SO_EXT = so
	DEFAULT_LDFLAGS += -flat_namespace -undefined suppress
endif

ifeq ($(UNAME), Linux)
  SO_EXT = so
	DEFAULT_CFLAGS += -DLINUX
endif

all: priv/nativerefs.$(SO_EXT)


CFLAGS += $(DEFAULT_CFLAGS)
LDFLAGS += $(DEFAULT_LDFLAGS)

c_src/nativerefs.o: c_src/nativerefs.c
	@$(CC) $< $(CPPFLAGS) $(CFLAGS) -c -o $@

priv/nativerefs.$(SO_EXT): c_src/nativerefs.o | priv/
	@$(CXX) $^ $(LDFLAGS) -o $@ 2> >(grep -v "corrupt .drectve at end of def file")

clean:
	@rm -f c_src/*.o priv/*.$(SO_EXT)

priv/:
	mkdir priv/

