defmodule MyServer.Database do
  use GenServer

  # Initialize the GenServer
  def start_link() do
    GenServer.start_link(__MODULE__, :ets.new(:my_table, [:public]), name: __MODULE__)
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

  def search(data_list, criteria_list) when is_list(criteria_list) do
    matching_values =
      Enum.filter(data_list, fn value ->
        value_matches_criteria({:ok, value}, criteria_list)
      end)

    case matching_values do
      [] ->
        {:error, "No matching values found."}

      _ ->
        {:ok, matching_values}
    end
  end

  # Modify the value_matches_criteria function to accept {:ok, value} directly
  defp value_matches_criteria({:ok, value}, criteria_list) when is_list(criteria_list) do
    Enum.any?(criteria_list, fn {key, expected_value} ->
      case Map.get(value, key) do
        nil -> false
        actual_value when actual_value == expected_value -> true
        _ -> false
      end
    end)
  end

  # Get all values from the database
  def get_all_values(server) do
    GenServer.call(server, :get_all_values)
  end

  # Add the corresponding GenServer callback for handling :get_all_values
  def handle_call(:get_all_values, _from, ets) do
    values = :ets.tab2list(ets)
    {:reply, values, ets}
  end
end
