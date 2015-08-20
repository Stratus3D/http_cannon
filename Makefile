PROJECT = http_cannon

ERLC_OPTS= +'{parse_transform, lager_transform}'

#-D_REENTRANT=PTHREADS
CFLAGS = "-I$(LUA)/include/"
LDFLAGS = "-L$(LUA)/lib -llua -lm -ldl"

# Options for the shell command in development
#SHELL_DEPS
SHELL_OPTS = -eval "application:ensure_all_started(http_cannon)."
#SHELL_PATH

# Packages
DEPS = lager

dep_lager = git https://github.com/basho/lager.git 3.0.1

include erlang.mk
