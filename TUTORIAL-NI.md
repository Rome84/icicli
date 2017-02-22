```

     /$$$$$$           /$$                                               /$$ /$$
    |_  $$_/          |__/                                              | $$|__/
      | $$    /$$$$$$$ /$$ /$$$$$$$   /$$$$$$   /$$$$$$         /$$$$$$$| $$ /$$
      | $$   /$$_____/| $$| $$__  $$ /$$__  $$ |____  $$       /$$_____/| $$| $$
      | $$  | $$      | $$| $$  \ $$| $$  \ $$  /$$$$$$$      | $$      | $$| $$
      | $$  | $$      | $$| $$  | $$| $$  | $$ /$$__  $$      | $$      | $$| $$
     /$$$$$$|  $$$$$$$| $$| $$  | $$|  $$$$$$$|  $$$$$$$      |  $$$$$$$| $$| $$
    |______/ \_______/|__/|__/  |__/ \____  $$ \_______/       \_______/|__/|__/
                                     /$$  \ $$
                                    |  $$$$$$/
                                     \______/
```

# Tutorial in non-interactive mode

All non-interactive operations will result in the process to exit with either success or failure
code depending on whether operation completed successfully or not. This also applies to any `GET`
operations when user is trying to query certain data from Icinga - in cases when object is not
found process will exit with non-success code.

## Manipulating Host Groups

Host groups can easily represent environments (TEST, SYST or PROD) - this is only an example of how
it can be used, so feel free to choose different way if you feel like.

Let's check that group does not exist before we start:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get host-group test-group
```

This request should return `Nothing` in case host group does not exist. It will also exit the
process with non-success code. Such that `%ERRORLEVEL%` (on Windows) will be `1`.

Create a new group:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    new host-group test-group [] "Test Group" []
```

This should result in success exit code and print out `Status {statusCode = 200, statusMessage = "OK"}`.

If we try to create this group again process will exit with non-success code and print out
something like this: `Status {statusCode = 503, statusMessage = "Unhandled exception"}`.

Get the group that we have just created:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get host-group test-group
```

This will pretty-print out following json:

```{.json}
[
  {
    "groups": [],
    "display_name": "Test Group",
    "templates": [
      "test-group"
    ],
    "active": true,
    "name": "test-group",
    "version": 1.462346266212311e9,
    "package": "_api",
    "type": "HostGroup",
    "vars": {
      "routing_key": "routing-key"
    },
    "paused": false
  }
]
```

Let's create child group (group hierarchy works more like tagging in Icinga):

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    new host-group child-group [] "Child Group" [\"test-group\"]
```

This should print out `Status {statusCode = 200, statusMessage = "OK"}`.

> Don't be surprised if you won't find host groups you have just create on Icinga Web portal. They
> will remain hidden until you will create at least one single host inside them.

## Manipulating Hosts

Let's create new host under child-group we have previously created:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    new host test-host [] "Test Host" [\"child-group\"] 127.0.0.1 hostalive [(\"os\",\"Windows\")]
```

This prints out expected `Status {statusCode = 200, statusMessage = "OK"}` and exits with success
code.

Get just create host:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get host test-host
```

And this pretty-prints json (here is just an excerpt from long response message):

```{.json}
[
  {
    "check_period": "",
    "max_check_attempts": 3,
    "groups": [
      "child-group",
      "windows-servers",
      "test-group"
    ],
    "check_attempt": 1,
    "event_command": "",
    "enable_passive_checks": true,
    "flapping_last_change": 0,
    ...
    "active": true,
    "name": "test-host",
    ...
    "vars": {
      "routing_key": "routing-key",
      "os": "Windows"
    },
    "paused": false
  }
]
```

> Note that `test-host` we just created belongs to two groups: `test-group` and `child-group`. This
> is why tagging comes to mind when one wants to describe how host groups work in Icinga.

## Manipulating Services

> We will skip service groups manipulation - main principles are exactly like with host groups.

Let's create a new service under `test-host` we have just created:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    new service test-host test-svc [] "Test Service" [] passive
```

This prints out `Status {statusCode = 200, statusMessage = "OK"}` with success exit code.

And again like in the example in host groups section when we try to create service that has already
been created we get `Status {statusCode = 503, statusMessage = "Unhandled exception"}` that also
results in the failure exit code.

To retrieve information about our services:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get service test-host test-svc
```

This will pretty-print following json:

```{.json}
[
  {
    "max_check_attempts": 3,
    "groups": [],
    "check_attempt": 1,
    "enable_passive_checks": true,
    "flapping_positive": 225,
    "force_next_notification": false,
    "flapping_last_change": 1.462360109991794e9,
    "state": 3,
    "enable_perfdata": true,
    "last_state_warning": 0,
    "acknowledgement": 0,
    "display_name": "Test Service",
    "last_reachable": true,
    "last_in_downtime": false,
    "last_check_result": {
      "state": 3,
      "command": [
        "/usr/local/nagios/libexec//check_dummy",
        3.0,
        "No Passive Check Result Received."
      ],
      "schedule_start": 1.4623604099899998e9,
      "execution_end": 1.462360109991689e9,
      "active": true,
      "check_source": "HOSTNAME",
      "performance_data": [],
      "output": "UNKNOWN: No Passive Check Result Received.",
      "schedule_end": 1.462360109991756e9,
      "execution_start": 1.462360109990325e9,
      "exit_status": 3
    },
    "enable_notifications": true,
    "retry_interval": 60,
    "enable_active_checks": true,
    "volatile": false,
    "templates": [
      "test-svc"
    ],
    "icon_image": "",
    "flapping_threshold": 30,
    "last_check": 1.462360109991756e9,
    "check_command": "passive",
    "next_check": 1.4623604099899998e9,
    "check_interval": 300,
    "zone": "",
    "last_state_ok": 1.462359435605156e9,
    "last_state_unreachable": 0,
    "force_next_check": false,
    "active": true,
    "name": "test-svc",
    "notes_url": "",
    "flapping_negative": 672,
    "version": 1.462359131517459e9,
    "last_state": 3,
    "action_url": "",
    "package": "_api",
    "acknowledgement_expiry": 0,
    "last_hard_state": 3,
    "ha_mode": 0,
    "enable_flapping": false,
    "last_state_unknown": 1.462360109991775e9,
    "enable_event_handler": false,
    "icon_image_alt": "",
    "last_state_type": 1,
    "state_type": 1,
    "type": "Service",
    "host_name": "test-host",
    "flapping": false,
    "notes": "",
    "last_state_change": 1.462359735606674e9,
    "command_endpoint": "",
    "last_hard_state_change": 1.462359869992053e9,
    "vars": {
      "routing_key": "routing-key"
    },
    "last_state_critical": 0,
    "paused": false
  }
]
```

Since we have created passive service, it is expected that we also report status of this service to
Icinga. This is how it can be done:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    run service test-host test-svc 0 "SUCCESS: all looks normal"
```

This will result in `Status {statusCode = 200, statusMessage = "OK"}` and mark your service as
green which means that last check was successful. You can report another status then success (we
passed `0` for that), here is the table:

| Exit code | Status        |
|:---------:|:-------------:|
|     0     |   OK          |
|     1     |   WARNING     |
|     2     |   ERROR       |
|     3     |   UNKNOWN     |

## Manipulating Notifications

If one wants to make sure changes in status of a service are getting noticed, one needs to create
notification. Notifications can be created not only for services but also for hosts (state changes
for hosts typically means - `down` or `up`).

Let's create notification for `test-svc` we have created in previous section:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    new notification test-host test-svc test-notif [\"notify-victorops-service\"] victorops-svc-notification [\"routing-key\"] [] []
```

This creates `test-notif` notification that is attached to `test-svc` and prints out `Status {statusCode = 200, statusMessage = "OK"}`.

> Note that in this case we are creating notification that uses special `notify-victorops-service`
> template that is custom for our particular Icinga setup. According to this setup and in order to
> be able to push notifications from Icinga to VictorOps we will also need to provide `routing-key`
> user. Your setup will most likely look different, but main principles on how to create
> notifications will still apply.

Let's now retrieve information about our notification from Icinga:

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get notification test-host test-svc test-notif
```

This pretty-prints json:

```{.json}
[
  {
    "states": [
      2,
      8,
      4,
      1
    ],
    "notified_users": [
      "VictorOps"
    ],
    "command": "victorops-svc-notification",
    "users": [
      "VictorOps"
    ],
    "type_filter_real": 112,
    "next_notification": 1.462362094110799e9,
    "period": "24x7",
    "last_notification": 1.462360294114053e9,
    "templates": [
      "test-notif",
      "notify-victorops-service",
      "notify-victorops"
    ],
    "types": [
      32,
      16,
      64
    ],
    "zone": "",
    "last_problem_notification": 1.462360294110898e9,
    "active": true,
    "name": "test-notif",
    "interval": 1800,
    "version": 1.462360291016023e9,
    "package": "_api",
    "pause": false,
    "ha_mode": 0,
    "state_filter_real": 15,
    "service_name": "test-svc",
    "type": "Notification",
    "host_name": "test-host",
    "command_endpoint": "",
    "vars": {
      "routing_key": "routing-key"
    },
    "notification_number": 1
  }
]
```

## Deleting Objects

Any object that we have created up until this moment can also be removed. The tool will attempt to
remove objects recursively, meaning that if we will try to remove `test-group` that is the parent
of all other objects we have created, Icinga should automatically delete all of its children.

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    del host-group test-group
```

This prints out usual `Status {statusCode = 200, statusMessage = "OK"}`.

Let's now try to see if we can get `test-group` or any of its children.

```{.bash}
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
  get host-group test-group

$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get host-group child-group

$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get host test-host

$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get service test-host test-svc

$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
    get notification test-host test-svc test-notif
```

Each and every of these requests will result in `Nothing` as well as failure exit code.
