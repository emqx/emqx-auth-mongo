PROJECT = emqttd_auth_mongo
PROJECT_DESCRIPTION = emqttd Authentication/ACL against MongoDB
PROJECT_VERSION = 1.1

DEPS = mongodb ecpool emqttd 

dep_mongodb = git https://github.com/comtihon/mongodb-erlang v0.7.9
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
