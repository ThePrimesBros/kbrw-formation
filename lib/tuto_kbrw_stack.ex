defmodule TutoKbrwStack do
  use Application

  @impl true
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Application.put_env(
      :reaxt,
      :global_config,
      Map.merge(
        Application.get_env(:reaxt, :global_config),
        %{localhost: "http://0.0.0.0:8080"}
      )
    )

    Reaxt.reload()

    children = [
      {Plug.Cowboy, scheme: :http, plug: Server.Router, options: [port: 8080]},
      worker(MyServer.Database, [])
    ]

    opts = [strategy: :one_for_one, name: TutoKbrwStack.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
