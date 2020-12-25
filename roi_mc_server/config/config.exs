# This file is responsible for configuring your umbrella
# and **all applications** and their dependencies with the
# help of the Config module.
#
# Note that all applications in your umbrella share the
# same configuration and dependencies, which is why they
# all use the same configuration file. If you want different
# configurations or dependencies per app, it is best to
# move said applications out of the umbrella.
import Config

# Sample configuration:
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]
#

config :middle_controller,
  websocket_server_conf: %{
    port: 8958,
    pre_start_process: 1,
    tls?: false,
    certfile: "C:\\Users\\Administrator\\Downloads\\1480091f9d0af64a.pem",
    keyfile: "C:\\Users\\Administrator\\Downloads\\generated-private-key-milestonepms.txt",
  },
  http_server_conf: %{
    port: 8948,
    pre_start_process: 1,
    tls?: false,
    certfile: "C:\\Users\\Administrator\\Downloads\\1480091f9d0af64a.pem",
    keyfile: "C:\\Users\\Administrator\\Downloads\\generated-private-key-milestonepms.txt",
  },
  socket_server_conf: %{
    port: 8938,
    head_length: 2,
    pre_start_process: 1,
  }
