defmodule Server.Router do
  # use Server.TheCreator
  use Plug.Router

  # my_error code: 404, content: "Custom error message enfin si on veut"

  # my_get "/" do
  #   {200, "Welcome to the new world of Plugs!"}
  # end

  # my_get "/me" do
  #   {200, "You are the Second One."}
  # end
  plug(Plug.Logger)

  plug(Plug.Static, from: "priv/static", at: "/static")
  plug(:format_header)
  plug(:match)
  plug(:dispatch)

  get "/search" do
    # Fetch query parameters
    params = Plug.Conn.fetch_query_params(conn)
    id = params.query_params["id"]
    value = params.query_params["value"]

    # Define the search criteria as a map
    criteria = [{id, value}]

    case MyServer.Database.search(MyServer.Database, criteria) do
      {:ok, db_value} ->
        send_resp(conn, 200, "Matching values: #{inspect(db_value)}")

      {:error, _reason} ->
        send_resp(conn, 404, "No matching values found.")
    end
  end

  post "/add_value" do
    case Plug.Conn.read_body(conn) do
      {:ok, json_body, _conn} ->
        case JSON.decode(json_body) do
          {:ok, json_map} ->
            case extract_key_value(json_map) do
              {:ok, {key, value}} ->
                case MyServer.Database.add_key_value(MyServer.Database, key, value) do
                  :ok ->
                    send_resp(conn, 200, "Value added successfully")

                  {:error, reason} ->
                    send_resp(conn, 500, "Error adding value: #{reason}")
                end

              {:error, _} ->
                send_resp(conn, 400, "Missing key or value in JSON body")
            end

          {:ok, _} ->
            send_resp(conn, 400, "Invalid JSON data in request body")

          {:error, _} ->
            send_resp(conn, 400, "Invalid JSON data in request body")
        end

      {:error, _} ->
        send_resp(conn, 400, "Invalid JSON data in request body")
    end
  end

  defp extract_key_value(json_map) when is_map(json_map) do
    case Map.keys(json_map) do
      [key] ->
        case Map.get(json_map, key) do
          value when is_binary(value) or is_integer(value) or is_boolean(value) ->
            {:ok, {key, value}}

          _ ->
            {:error, "Invalid value type in JSON body for key #{key}"}
        end

      _ ->
        {:error, "Invalid number of keys in JSON body"}
    end
  end

  defp extract_key_value(_), do: {:error, "Invalid JSON body format"}

  get "/get_value/:key" do
    key = conn.params["key"]
    IO.inspect(MyServer.Database.get_value(MyServer.Database, key))

    case MyServer.Database.get_value(MyServer.Database, key) do
      {key, value} ->
        send_resp(conn, 200, "Value for key=#{key}: #{value}")

      {:error, _reason} ->
        send_resp(conn, 404, "Value not found for key=#{key}")
    end
  end

  # # Get all elements
  get "/orders" do
    case MyServer.Database.get_all_values(MyServer.Database) do
      values when length(values) > 0 ->
        values_str =
          Enum.map(values, fn {key, value} ->
            "#{key}: #{value}"
          end)

        values_str = Enum.join(values_str, ", ")
        send_resp(conn, 200, "All values: #{values_str}")

      [] ->
        send_resp(conn, 200, "No values found")

      {:error, _reason} ->
        send_resp(conn, 500, "Error fetching values")
    end
  end

  put "/update_value/:key" do
    key = conn.params["key"]

    case Plug.Conn.read_body(conn) do
      {:ok, json_body, _conn} ->
        case JSON.decode(json_body) do
          {:ok, json_map} ->
            case extract_key_value(json_map) do
              {:ok, {new_key, new_value}} ->
                case MyServer.Database.update_value(
                       MyServer.Database,
                       key,
                       new_value
                     ) do
                  :ok ->
                    send_resp(conn, 200, "Value updated successfully")

                  {:error, reason} ->
                    send_resp(conn, 500, "Error updating value: #{reason}")
                end

              {:error, _} ->
                send_resp(conn, 400, "Missing key or value in JSON body")
            end

          {:ok, _} ->
            send_resp(conn, 400, "Invalid JSON data in request body")

          {:error, _} ->
            send_resp(conn, 400, "Invalid JSON data in request body")
        end

      {:error, _} ->
        send_resp(conn, 400, "Invalid JSON data in request body")
    end
  end

  delete "/delete_value/:key" do
    key = conn.params["key"]

    case MyServer.Database.delete_key(MyServer.Database, key) do
      :ok ->
        send_resp(conn, 200, "Value deleted successfully")

      {:error, _reason} ->
        send_resp(conn, 500, "Error deleting value")
    end
  end

  @orders File.read!("./orders_chunk0.json") |> Poison.decode! |> Enum.take(20)

  get "/api/orders" do
    conn = fetch_query_params(conn)
           |> format_header
           |> put_resp_content_type("application/json")

    json = %{results: @orders, total: 20} |> Poison.encode!
    send_resp(conn, 200, json)
  end


  get "/api/order/:id" do
    json = %{id: "nat_order#{id}", id: id} |> Poison.encode!
    send_resp(conn, 200, json)
  end


  match(_, do: send_file(conn, 200, "priv/static/index.html") )

  defp format_header(conn, _ \\ 0) do
    conn
    |> Plug.Conn.put_resp_header("access-control-allow-origin", "*")
    |> Plug.Conn.put_resp_header("Access-Control-Allow-Headers", "access-control-allow-origin")
    #|> put_resp_content_type("text/plain")
  end
end
