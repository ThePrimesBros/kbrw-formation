defmodule MyServer.FSMSupervisor do
  use DynamicSupervisor
  use Supervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_child(order_id) do
    spec = %{
      id: order_id,
      start: {MyServer.Payment, :start_link, [order_id]},
    }

    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
