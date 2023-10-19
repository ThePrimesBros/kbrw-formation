defmodule JsonLoader do
  alias Poison

  defmacro __using__(_opts) do
  end

  def upload_to_riak(json_file) do
    case File.read(json_file) do
      {:ok, json_string} ->
        case Poison.decode(json_string) do
          {:ok, json_data} ->
            Task.async(fn -> upload_all_to_riak(json_data) end) |> Task.await(:infinity)

            IO.puts("Data uploaded to Riak successfully")
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

  defp upload_all_to_riak(json_data) do
    Enum.each(json_data, fn x ->
      data = Poison.encode!(x)
      # IO.inspect(x)
      MyServer.Riak.add_object(data)
    end)
  end
end
