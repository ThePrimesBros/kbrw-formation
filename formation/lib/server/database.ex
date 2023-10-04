defmodule MyServer.Database do
  use GenServer

  # Initialize the GenServer
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ets.new(:my_table, opts), name: __MODULE__)
  end

  # CRUD operations
  def add_key_value(server, key, value) do
    GenServer.cast(server, {:add, key, value})
  end

  def get_value(server, key) do
    GenServer.call(server, {:get, key})
  end

  def update_value(server, key, value) do
    GenServer.cast(server, {:update, key, value})
  end

  def delete_key(server, key) do
    GenServer.cast(server, {:delete, key})
  end

  # GenServer callbacks
  def init(ets) do
    {:ok, ets}
  end

  def handle_cast({:add, key, value}, ets) do
    :ets.insert(ets, {key, value})
    {:noreply, ets}
  end

  def handle_cast({:update, key, value}, ets) do
    :ets.insert(ets, {key, value})
    {:noreply, ets}
  end

  def handle_cast({:delete, key}, ets) do
    :ets.delete(ets, key)
    {:noreply, ets}
  end

  def handle_call({:get, key}, _from, ets) do
    value = :ets.lookup(ets, key)
    {:reply, hd(value), ets}
  end

  def search(database, criteria) when is_list(criteria) do
    # Filter orders based on criteria
    Enum.filter(database, fn order ->
      # Check if any of the criteria matches the order
      Enum.any?(criteria, fn {key, value} ->
        Map.get(order, key) == value
      end)
    end)
  end
end
