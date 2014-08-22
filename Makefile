PROJECT = fiar

DEPS = sync

dep_sync = git https://github.com/rustyio/sync.git master

include erlang.mk

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync

devtests: tests
	open logs/index.html