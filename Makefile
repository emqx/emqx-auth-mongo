PROJECT = emq_auth_mongo
PROJECT_DESCRIPTION = Authentication/ACL with MongoDB
PROJECT_VERSION = 2.0.1

DEPS = mongodb ecpool

dep_mongodb  = git https://github.com/comtihon/mongodb-erlang
dep_ecpool   = git https://github.com/emqtt/ecpool master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_mongo.conf -i priv/emq_auth_mongo.schema -d data

