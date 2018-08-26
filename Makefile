PROJECT = emqx_auth_mongo
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with MongoDB
PROJECT_VERSION = 3.0

DEPS = mongodb ecpool clique emqx_passwd
dep_mongodb = git https://github.com/emqtt/mongodb-erlang v3.0.7
dep_ecpool  = git https://github.com/emqtt/ecpool master
dep_clique  = git https://github.com/emqx/clique
dep_emqx_passwd = git https://github.com/emqx/emqx-passwd emqx30

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emqx_auth_username
dep_emqx_auth_username = git https://github.com/emqx/emqx-auth-username emqx30

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_mongo.conf -i priv/emqx_auth_mongo.schema -d data
