defmodule TutoKbrwStack do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      {MyServer.ServSupervisor, []}
    ]

    opts = [strategy: :one_for_one, name: TutoKbrwStack.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
