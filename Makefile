SHELL = bash
UNAME := $(shell uname)
VPATH = priv


#------------------------------------------------------------
# Find erlang install
space :=
space +=
s+ = $(subst $(space),+,$1)
+s = $(subst +,\$(space),$1)
erl_root_dir = $(shell erl -eval 'io:format(standard_error, "~s", [os:getenv("ROOTDIR")]), erlang:halt().' 2>&1 >/dev/null)
ifeq ($(UNAME), MINGW64_NT-10.0)
  ERLDIR = $(call +s,$(addsuffix ../.., $(dir $(call s+,$(shell which erl)))))
else
  ifeq ($(_KERL_ACTIVE_DIR),)
    ERLDIR = $(call erl_root_dir)
  else
    ERLDIR = $(_KERL_ACTIVE_DIR)
  endif
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

all: priv/native_refs.$(SO_EXT)


CFLAGS += $(DEFAULT_CFLAGS)
LDFLAGS += $(DEFAULT_LDFLAGS)

c_src/native_refs.o: c_src/native_refs.c
	@$(CC) $< $(CPPFLAGS) $(CFLAGS) -c -o $@

priv/native_refs.$(SO_EXT): c_src/native_refs.o | priv/
	@$(CXX) $^ $(LDFLAGS) -o $@ 2> >(grep -v "corrupt .drectve at end of def file")

clean:
	@rm -f c_src/*.o priv/*.$(SO_EXT)

priv/:
	mkdir priv/

