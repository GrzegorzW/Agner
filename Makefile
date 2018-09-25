PROJECT = agner
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy gun jiffy mochiweb lager
dep_cowboy_commit = master

DEP_PLUGINS = cowboy

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

dep_jiffy = git https://github.com/davisp/jiffy