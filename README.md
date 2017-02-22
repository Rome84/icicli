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

# IcingaCli

Icinga 2 command line interface.

## Developer Environment

[Stack](http://haskellstack.org) is the easiest and pain free way of building this project.

```{.bash}
# building project (debug flag can be skipped)
stack build --test --file-watch --flag *:debug

# run icinga client
stack exec icicli
```

## Interactive / non-interactive modes

The tool can work in interactive and non-interactive modes. In order to enter interactive mode you
need to supply url of Icinga API, user name and password (optional) and routing key (optional).

```
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key"
```

Non-interactive mode will require to supply further commands and parameters after required url,
user name and password (optional) and routing key (optiona). For example following will print out
help information and quit the process:

```
$ icicli "https://icinga2-api:5665/v1" "username" "password" "routing-key" help
```

All commands issued in non-interactive mode will result in `icicli` process exiting with exit code
that will determine success or failure. This will be useful in scenarious when `icicli` is used in
CI/CD pipelines reporting status to Icinga.

## Examples

In this examples we will use non-interactive mode (it is all also possible in interactive mode):

* [Tutorial in non-interactive mode](TUTORIAL-NI.md)
* [Tutorial in interactive mode](TUTORIAL-IN.md)
