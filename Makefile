PROJECT = agner
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy gun jiffy mochiweb
dep_cowboy_commit = master

DEP_PLUGINS = cowboy

include erlang.mk

dep_jiffy = git https://github.com/davisp/jiffy