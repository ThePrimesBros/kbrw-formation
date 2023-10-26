defimpl ExFSM.Machine.State, for: Map do
  def state_name(order), do: String.to_atom(order["status"]["state"])
  def set_state_name(order, name), do: put_in(order, ["status", "state"], Atom.to_string(name))
  def handlers(order) do
    [MyServer.Payment]
  end
end

defmodule MyServer.Payment do
  use GenServer, shutdown: 300_000
  use ExFSM
  # Client functions

  defmacro __using__(_opts) do
  end

  def start_link(order) do
    GenServer.start_link(__MODULE__, order)

    # GenServer.start_link(__MODULE__, {order, %{}}, name: {:global, {:via, Registry, {FSM.Registry, :order_registry}}})
  end

  def handle_cast(order, server) do
    # Use ExFSM to perform the transition
    IO.inspect(order)
    {:next_state, updated_order} =
    case ExFSM.Machine.State.state_name(MyServer.Riak.get(order)) do
      :not_verified -> ExFSM.Machine.event(MyServer.Riak.get(order), {:verification, []})
      :init -> ExFSM.Machine.event(MyServer.Riak.get(order), {:process_payment, []})
      _ -> :action_unavailable
    end

    # Update the order in your data store (e.g., Riak)
    # Replace this with your actual data storage logic
    MyServer.Riak.update_key(order, updated_order)

    {:noreply, {updated_order, %{}}}
  end

  def handle_call(:shutdown, _from, state) do
    # Handle graceful shutdown
    {:stop, :normal, state}
  end

  # Server functions

  @impl true
  def init(order) do
    {:ok, order}
  end

  def update_order(server, order) do
    GenServer.cast(server, order)
  end

  deftrans init({:process_payment, []}, order) do
    {:next_state, :not_verified, order}
  end

  deftrans not_verified({:verification, []}, order) do
    {:next_state, :finished, order}
  end
end
