defmodule JsonLoader do
  alias Poison

  def load_to_database(database, json_file) do
    case File.read(json_file) do
      {:ok, json_string} ->
        case Poison.decode(json_string) do
          {:ok, json_data} ->
            Enum.each(json_data, fn x ->
              order_id = Integer.to_string(Enum.count(json_data) + 1)
              MyServer.Database.add_key_value(database, order_id, x)
            end)
            IO.puts("Data loaded successfully")

          {:error, _} ->
            IO.puts("Failed to decode JSON data from #{json_file}")
        end

      {:error, _} ->
        IO.puts("Failed to read JSON file: #{json_file}")
    end
  end
end
