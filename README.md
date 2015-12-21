# emqtt_mongodb

## Configure Plugin

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
