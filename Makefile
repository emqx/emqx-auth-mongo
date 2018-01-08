PROJECT = emqx_auth_mongo
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with MongoDB
PROJECT_VERSION = 2.4.1

DEPS = mongodb ecpool clique
dep_mongodb = git https://github.com/emqtt/mongodb-erlang v3.0.3
dep_ecpool  = git https://github.com/emqtt/ecpool master
dep_clique  = git https://github.com/emqtt/clique

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx-enterprise
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emqttc emqx_auth_username
dep_emqttc = git https://github.com/emqtt/emqttc
dep_emqx_auth_username = git https://github.com/emqtt/emq-auth-username X

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_mongo.conf -i priv/emqx_auth_mongo.schema -d data

