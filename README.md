# emqtt_mongodb

## Overview

emqttd Authentication with MongoDB

## 配置插件

File: etc/plugin.config

```erlang
[
  {emqttd_auth_mongodb, [
	{database, "db0"},
	{collection, "mqtt_user"},
    {password_hash, sha256},
  ]}
].
```

## 根据插件配置参数创建MongoDB数据库和集合并为集合字段username创建索引

```sql
use db0
db.createCollection("mqtt_user")
db.mqtt_user.ensureIndex({"username":1})
```

## mqtt_user集合结构

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
