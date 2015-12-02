PROJECT = fiar

DEPS = sumo_db lager eper sync cowlib ranch cowboy lasse jiffy shotgun katana

dep_cowlib = git https://github.com/ninenines/cowlib.git 1.0.0
dep_ranch = git https://github.com/ninenines/ranch.git 1.0.0
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.0
dep_jiffy = git https://github.com/davisp/jiffy.git 0.11.3
dep_sumo_db = git https://github.com/inaka/sumo_db.git 0.1.2
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_lasse = git https://github.com/inaka/lasse.git 1.0.1
dep_eper = git https://github.com/massemanet/eper.git 35636bc4de07bc803ea4fc9731fab005d0378c2b

dep_katana = git https://github.com/inaka/erlang-katana 0.2.7
dep_shotgun = git https://github.com/inaka/shotgun 0.1.12

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk
ERLC_OPTS := +warn_unused_vars +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_untyped_record +debug_info

ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS = -cover test/fiar.coverspec -erl_args -config config/test.config
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync -s fiar -config config/test.config


test-shell: build-ct-suites app
	erl -name fiar@`hostname` -pa ebin -pa deps/*/ebin -pa test -s sync -s fiar -s shotgun -config config/test.config

devtests: tests
	open logs/index.html
