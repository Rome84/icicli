module Helpers.Usage
  ( usage'
  , usage
  ) where

asciiArt :: IO ()
asciiArt = do
  putStrLn ""
  putStrLn ""
  putStrLn "     /$$$$$$           /$$                                               /$$ /$$"
  putStrLn "    |_  $$_/          |__/                                              | $$|__/"
  putStrLn "      | $$    /$$$$$$$ /$$ /$$$$$$$   /$$$$$$   /$$$$$$         /$$$$$$$| $$ /$$"
  putStrLn "      | $$   /$$_____/| $$| $$__  $$ /$$__  $$ |____  $$       /$$_____/| $$| $$"
  putStrLn "      | $$  | $$      | $$| $$  \\ $$| $$  \\ $$  /$$$$$$$      | $$      | $$| $$"
  putStrLn "      | $$  | $$      | $$| $$  | $$| $$  | $$ /$$__  $$      | $$      | $$| $$"
  putStrLn "     /$$$$$$|  $$$$$$$| $$| $$  | $$|  $$$$$$$|  $$$$$$$      |  $$$$$$$| $$| $$"
  putStrLn "    |______/ \\_______/|__/|__/  |__/ \\____  $$ \\_______/       \\_______/|__/|__/"
  putStrLn "                                     /$$  \\ $$"
  putStrLn "                                    |  $$$$$$/"
  putStrLn "                                     \\______/"

usage' :: IO ()
usage' = do
  asciiArt
  putStrLn ""
  putStrLn "  ?|help"
  putStrLn ""
  putStrLn "  url [username, password] [routing-key]          -- enter interactive mode"
  putStrLn "  url [username, password] [routing-key] status"
  putStrLn ""
  putStrLn "  -- all following commands must be prefixed with non-interactive arguments:"
  putStrLn "  --    url [username, password] [routing-key]"
  putStrLn ""
  putStrLn "  [..] get host-group <name>"
  putStrLn "  [..] new host-group <name> <templates:list> <displayName> <groups:list> [<vars:map>]"
  putStrLn "  [..] set host-group <name> <displayName> [<vars:map>]"
  putStrLn "  [..] del host-group <name>"
  putStrLn ""
  putStrLn "  [..] get host <name>"
  putStrLn "  [..] new host <name> <templates:list> <displayName> <groups:list> <address> <checkCommand> [<vars:map>]"
  putStrLn "  [..] set host <name> <activeChecks:flag> <passiveChecks:flag> <perfData:flag> <notifications:flag> <eventHandler:flag> <flapDetection:flag>"
  putStrLn "  [..] set host <name> <checkInterval:secs> <retryInterval:secs> <maxCheckAttempts:int> <checkCommand> [<vars:map>]"
  putStrLn "  [..] set host <name> <vars:map>"
  putStrLn "  [..] del host <name>"
  putStrLn ""
  putStrLn "  [..] get service-group <name>"
  putStrLn "  [..] new service-group <name> <templates:list> <displayName> <groups:list> [<vars:map>]"
  putStrLn "  [..] set service-group <name> <displayName> [<vars:map>]"
  putStrLn "  [..] del service-group <name>"
  putStrLn ""
  putStrLn "  [..] get service <hostname> <name>"
  putStrLn "  [..] new service <hostname> <name> <templates:list> <displayName> <groups:list> <checkCommand> [<vars:map>]"
  putStrLn "  [..] set service <hostname> <name> <activeChecks:flag> <passiveChecks:flag> <perfData:flag> <notifications:flag> <eventHandler:flag> <flapDetection:flag>"
  putStrLn "  [..] set service <hostname> <name> <checkInterval:secs> <retryInterval:secs> <maxCheckAttempts:int> <checkCommand> [<vars:map>]"
  putStrLn "  [..] set service <hostname> <name> <vars:map>"
  putStrLn "  [..] run service <hostname> <name> <exitStatus:int> <pluginOutput> [<perfData:list>]"
  putStrLn "  [..] del service <hostname> <name>"
  putStrLn ""
  putStrLn "  [..] get notification <hostname> [<servicename>] <name>"
  putStrLn "  [..] new notification <hostname> [<servicename>] <name> <templates:list> <command> <users:list> <states:int-list> <vars:map>"
  putStrLn "  [..] set notification <hostname> [<servicename>] <name> <vars:map>"
  putStrLn "  [..] del notification <hostname> [<servicename>] <name>"
  putStrLn ""

usage :: IO ()
usage = do
  asciiArt
  putStrLn ""
  putStrLn "  ?|help"
  putStrLn "  quit|exit"
  putStrLn ""
  putStrLn "  status"
  putStrLn ""
  putStrLn "  get host-group <name>"
  putStrLn "  new host-group <name> <templates:list> <displayName> <groups:list> [<vars:map>]"
  putStrLn "  set host-group <name> <displayName> [<vars:map>]"
  putStrLn "  del host-group <name>"
  putStrLn ""
  putStrLn "  get host <name>"
  putStrLn "  new host <name> <templates:list> <displayName> <groups:list> <address> <checkCommand> [<vars:map>]"
  putStrLn "  set host <name> <activeChecks:flag> <passiveChecks:flag> <perfData:flag> <notifications:flag> <eventHandler:flag> <flapDetection:flag>"
  putStrLn "  set host <name> <checkInterval:secs> <retryInterval:secs> <maxCheckAttempts:int> <checkCommand> [<vars:map>]"
  putStrLn "  set host <name> <vars:map>"
  putStrLn "  del host <name>"
  putStrLn ""
  putStrLn "  get service-group <name>"
  putStrLn "  new service-group <name> <templates:list> <displayName> <groups:list> [<vars:map>]"
  putStrLn "  set service-group <name> <displayName> [<vars:map>]"
  putStrLn "  del service-group <name>"
  putStrLn ""
  putStrLn "  get service <hostname> <name>"
  putStrLn "  new service <hostname> <name> <templates:list> <displayName> <groups:list> <checkCommand> [<vars:map>]"
  putStrLn "  set service <hostname> <name> <activeChecks:flag> <passiveChecks:flag> <perfData:flag> <notifications:flag> <eventHandler:flag> <flapDetection:flag>"
  putStrLn "  set service <hostname> <name> <checkInterval:secs> <retryInterval:secs> <maxCheckAttempts:int> <checkCommand> [<vars:map>]"
  putStrLn "  set service <hostname> <name> <vars:map>"
  putStrLn "  run service <hostname> <name> <exitStatus:int> <pluginOutput> [<perfData:list>]"
  putStrLn "  del service <hostname> <name>"
  putStrLn ""
  putStrLn "  get notification <hostname> [<servicename>] <name>"
  putStrLn "  new notification <hostname> [<servicename>] <name> <templates:list> <command> <users:list> <states:int-list> <vars:map>"
  putStrLn "  set notification <hostname> [<servicename>] <name> <vars:map>"
  putStrLn "  del notification <hostname> [<servicename>] <name>"
  putStrLn ""
