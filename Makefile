PROJECT = http_cannon

#-D_REENTRANT=PTHREADS
CFLAGS = "-I$(LUA)/include/"
LDFLAGS = "-L$(LUA)/lib -llua -lm -ldl"

# Options for the shell command in development
#SHELL_DEPS
SHELL_OPTS = -s application start http_cannon
#SHELL_PATH

include erlang.mk
