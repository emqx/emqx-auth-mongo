
emq_auth_mongo
==============

Authentication with MongoDB

Build the Plugin
----------------

```
make & make tests
```

Configuration
-------------

File: etc/emq_auth_mongo.conf

```erlang
## Mongo Server
auth.mongo.server = 127.0.0.1:27017

## Mongo Pool Size
auth.mongo.pool = 8

## Mongo User
## auth.mongo.login = 

## Mongo Password
## auth.mongo.password = 

## Mongo Database
auth.mongo.database = mqtt

## authquery
auth.mongo.authquery.collection = mqtt_user

auth.mongo.authquery.password_field = password

auth.mongo.authquery.password_hash = sha256

auth.mongo.authquery.selector = username=%u

## superquery
auth.mongo.superquery.collection = mqtt_user

auth.mongo.superquery.super_field = is_superuser

auth.mongo.superquery.selector = username=%u

## aclquery
auth.mongo.aclquery.collection = mqtt_user

auth.mongo.aclquery.selector = username=%u

```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_auth_mongo
```

MongoDB Database
----------------

```
use mqtt
db.createCollection("mqtt_user")
db.createCollection("mqtt_acl")
db.mqtt_user.ensureIndex({"username":1})
```

mqtt_user Collection
--------------------

```
{
    username: "user",
    password: "password hash",
    is_superuser: boolean (true, false),
    created: "datetime"
}
```

For example:
```
db.mqtt_user.insert({username: "test", password: "password hash", is_superuser: false})
db.mqtt_user:insert({username: "root", is_superuser: true})
```

mqtt_acl Collection
-------------------

```
{
    username: "username",
    clientid: "clientid",
    publish: ["topic1", "topic2", ...],
    subscribe: ["subtop1", "subtop2", ...],
    pubsub: ["topic/#", "topic1", ...]
}
```

For example:

```
db.mqtt_acl.insert({username: "test", publish: ["t/1", "t/2"], subscribe: ["user/%u", "client/%c"]})
db.mqtt_acl.insert({username: "admin", pubsub: ["#"]})
```

License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>

