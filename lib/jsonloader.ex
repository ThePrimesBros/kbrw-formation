defmodule JsonLoader do
  alias Poison

  defmacro __using__(_opts) do

  end

  def load_to_database(database, json_file) do
    case File.read(json_file) do
      {:ok, json_string} ->
        case Poison.decode(json_string) do
          {:ok, json_data} ->
            Enum.each(json_data, fn x ->
              MyServer.Database.add_key_value(database, x["remoteid"], x)
            end)
            IO.puts("Data loaded successfully")
            {:ok}
          {:error, _} ->
            IO.puts("Failed to decode JSON data from #{json_file}")
            {:error}
        end

      {:error, _} ->
        IO.puts("Failed to read JSON file: #{json_file}")
        {:error}
    end
  end
end
