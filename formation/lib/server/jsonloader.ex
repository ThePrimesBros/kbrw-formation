defmodule JsonLoader do
  alias Poison

  # Load JSON data from a file and insert it into the database
  def load_to_database(database, json_file) do
    case File.read(json_file) do
      {:ok, json_string} ->
        case Poison.decode(json_string) do
          {:ok, json_data} ->
            Enum.each(json_data, fn {order_id, order_data} ->
              # Assuming your database is a Map, you can insert the data like this
              updated_database = Map.put(database, order_id, order_data)
              # You can also use ETS or other database options here
              # For example, for an ETS table: :ets.insert(table_name, {order_id, order_data})
            end)
          {:error, _} ->
            IO.puts("Failed to decode JSON data from #{json_file}")
        end

      {:error, _} ->
        IO.puts("Failed to read JSON file: #{json_file}")
    end
  end
end
