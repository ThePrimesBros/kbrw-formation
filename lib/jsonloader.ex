defmodule JsonLoader do
  alias Poison

  def load_to_database(database, json_file) do
    case File.read(json_file) do
      {:ok, json_string} ->
        case Poison.decode(json_string) do
          {:ok, json_data} ->
            Enum.each(json_data, fn x ->
              IO.inspect(x, label: "Mon id est")
              MyServer.Database.add_key_value(database, x["id"], x)
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
