defmodule MiddleController.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do

    children = [
      {MiddleController.Manager.CacheManager, nil},
      {MiddleController.Manager.SocketManager, nil},
      {MiddleController.Manager.WebSocketManager, nil},
      {MiddleController.Manager.HttpSocketManager, nil},
      {MiddleController.Manager.TickManager, nil},
      # Starts a worker by calling: MiddleController.Worker.start_link(arg)
      # {MiddleController.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MiddleController.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
