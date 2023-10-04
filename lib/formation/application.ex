defmodule Formation.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      {Plug.Cowboy, scheme: :http, plug: Server.Router, options: [port: 8080]},
      worker(MyServer.Database, [])
    ]

    opts = [strategy: :one_for_one, name: Formation.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
