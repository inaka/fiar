PROJECT = fiar

DEPS = sumo_db sync

dep_sumo_db = git https://github.com/inaka/sumo_db.git 0.1.2
dep_sync = git https://github.com/rustyio/sync.git master

include erlang.mk

CT_OPTS = -cover test/fiar.coverspec -erl_args -config config/test.config

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync -config config/test.config

devtests: tests
	open logs/index.html