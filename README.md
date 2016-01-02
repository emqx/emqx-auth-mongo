
# emqtt_mongodb

## Overview

emqttd MongoDB Authentication Plugin


## Configuration

File: etc/plugin.config

```erlang
[
  {emqttd_mongodb, [

    {mongodb_pool, [
      {pool_size, 4},
      {pool_type, random},
      {auto_reconnect, 3},

      %% Mongodb driver opts
      %% {login, ""},
      %% {password,""},
      {host, "localhost"},
      {port, 27019},
      {database, "db0"}
    ]},

    {user_collection, "mqtt_user"},

    %% hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256}

  ]}
].
```

## Mongodb

### database

```sql
use db0
db.createCollection("mqtt_user")
db.mqtt_user.ensureIndex({"username":1})
```

#### mqtt_user collection

```sql
{
  "username" : "1000",
  "password" : "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92",
  "sale" : 0,
  "created" : 0
}
```

## Build Plugin

This project is a plugin for emqttd broker. In emqttd project:

If the submodule exists:

```
git submodule update --remote
```

Orelse:

```
git submodule add https://github.com/emqtt/emqtt_mongodb.git plugins/emqtt_mongodb

make && make dist
```

## Load Plugin

```
./bin/emqttd_ctl plugins load emqttd_mongodb


