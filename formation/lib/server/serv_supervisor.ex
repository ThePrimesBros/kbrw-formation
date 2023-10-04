defmodule MyServer.ServSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      {MyServer.Database, []}  # Use `worker/3` for the GenServer
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
