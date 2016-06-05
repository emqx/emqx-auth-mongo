PROJECT = emqttd_auth_mongo
PROJECT_DESCRIPTION = Authentication/ACL with MongoDB
PROJECT_VERSION = 1.1

DEPS = mongodb ecpool emqttd 

dep_mongodb = git git://github.com/comtihon/mongodb-erlang.git v1.0.0
dep_ecpool  = git https://github.com/emqtt/ecpool master
dep_emqttd  = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
