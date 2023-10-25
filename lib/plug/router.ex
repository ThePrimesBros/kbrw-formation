defmodule Server.Router do
  # use Server.TheCreator
  use Plug.Router
  use JsonLoader
  require EEx
  use Ewebmachine.Builder.Resources

  plug(Plug.Static, at: "/public", from: :tutokbrwstack)
  EEx.function_from_file(:defp, :layout, "./web/layout.html.eex", [:render])

  plug(:match)
  plug(:dispatch)

  plug :resource_match
  plug Ewebmachine.Plug.Run
  plug Ewebmachine.Plug.Send
  # plug(Plug.Logger)

  get "/" do
    conn = fetch_query_params(conn)

    render =
      Reaxt.render!(
        :app,
        %{path: conn.request_path, cookies: conn.cookies, query: conn.params},
        30_000
      )

    send_resp(
      put_resp_header(conn, "content-type", "text/html;charset=utf-8"),
      render.param || 200,
      layout(render)
    )
  end

  # /api/orders - Supports pagination
  get "/api/orders" do
    conn =
      fetch_query_params(conn)
      |> format_header
      |> put_resp_content_type("application/json")

    page = Map.get(conn.query_params, "page", "0") |> String.to_integer()

    # Get the search query from the query parameters
    search_query = Map.get(conn.query_params, "q", "")
    per_page = 30

    search_query =
      case search_query do
        "" ->
          "type:nat_order"
        _ ->
          IO.inspect(search_query)
          search_query
      end

    # Update your Riak query to include the search criteria
    data =
      MyServer.Riak.search(
        "rbessonnier_orders_index",
        search_query,
        page,
        per_page
      )
      |> Enum.map(&Map.get(&1, "key"))

    # IO.inspect(data)
    # Extract only the values from the data array
    values = Enum.map(data, fn x -> MyServer.Riak.get(x) end)

    json = %{results: values } |> Poison.encode!()
    send_resp(conn, 200, json)
  end

  get "/api/order/:id" do
    data =
      MyServer.Riak.search("rbessonnier_orders_index", "remoteid:#{id}", 0, 30)
      |> Enum.map(&Map.get(&1, "key"))

    IO.inspect(hd(data))
    values = MyServer.Riak.get(hd(data))

    json = %{id: id, data: values} |> Poison.encode!()
    send_resp(conn, 200, json)
  end

  delete "/api/delete/:id" do
    case MyServer.Riak.delete_key(id) do
      :ok ->
        send_resp(conn, 200, "Order with ID #{id} deleted")

      :error ->
        send_resp(conn, 404, "Order not found")
    end
  end

  put "/api/pay/:id" do
    data =
      MyServer.Riak.search("rbessonnier_orders_index", "remoteid:#{id}", 0, 30)
      |> Enum.map(&Map.get(&1, "key"))
    #IO.inspect(Registry.lookup(FSM.Registry, {hd(data), :order_registry}))
    {:ok, pid} = MyServer.FSMSupervisor.start_child(hd(data))

    case MyServer.Payment.update_order(pid, hd(data)) do
      :ok ->
        send_resp(conn, 200, "Change status on the payment")

      :error ->
        send_resp(conn, 404, "Order not found")
    end
  end

  # match(_, do: send_file(conn, 200, "priv/static/index.html"))

  defp format_header(conn, _ \\ 0) do
    conn
    |> Plug.Conn.put_resp_header("access-control-allow-origin", "*")
    |> Plug.Conn.put_resp_header("Access-Control-Allow-Headers", "access-control-allow-origin")

    # |> put_resp_content_type("text/plain")
  end
end
