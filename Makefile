PROJECT = emqttd_auth_mongo
PROJECT_DESCRIPTION = Authentication/ACL with MongoDB
PROJECT_VERSION = 2.0

DEPS = mongodb ecpool gen_conf

dep_mongodb  = git https://github.com/comtihon/mongodb-erlang
dep_ecpool   = git https://github.com/emqtt/ecpool master
dep_gen_conf = git https://github.com/emqtt/gen_conf master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

