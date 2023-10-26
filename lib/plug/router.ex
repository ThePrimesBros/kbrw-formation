defmodule Server.Router do
  require EEx
  use Ewebmachine.Builder.Resources
  if Mix.env() == :dev, do: plug(Ewebmachine.Plug.Debug)

  plug(Plug.Static, at: "/public", from: :tutokbrwstack)
  EEx.function_from_file(:def, :layout, "./web/layout.html.eex", [:render])

  plug(:match)
  plug(:dispatch)

  plug(:resource_match)
  plug(Ewebmachine.Plug.Run)
  plug(Ewebmachine.Plug.Send)
  plug(Plug.Logger)

  resource "/api/orders" do
    %{}
  after
    plug(:fetch_query_params)
    plug(MyJSONApi)

    content_types_provided(do: ["application/json": :to_json])

    defh(to_json,
      do:
        Poison.encode!(
          MyServer.Riak.search(
            "rbessonnier_orders_index",
            Map.get(conn.query_params, "q", "type:nat_order"),
            Map.get(conn.query_params, "page", "0") |> String.to_integer(),
            30
          )
          |> Enum.map(&Map.get(&1, "key"))
          |> Enum.map(fn x -> MyServer.Riak.get(x) end)
        )
    )
  end

  resource "/api/order/:name" do
    %{name: name}
  after
    content_types_provided(do: ["application/json": :to_json])

    defh(to_json,
      do:
        Poison.encode!(
          MyServer.Riak.get(
            hd(
              MyServer.Riak.search("rbessonnier_orders_index", "remoteid:#{state.name}", 0, 30)
              |> Enum.map(&Map.get(&1, "key"))
            )
          )
        )
    )
  end

  resource "/api/delete/:name" do
    %{name: name}
  after
    allowed_methods(do: ["DELETE"])

    delete_resource(
      do:
        MyServer.Riak.delete_key(
          hd(
            MyServer.Riak.search("rbessonnier_orders_index", "remoteid:#{state.name}", 0, 30)
            |> Enum.map(&Map.get(&1, "key"))
          )
        )
    )
  end

  resource "/api/pay/:name" do
    %{name: name}
  after
    allowed_methods(do: ["PUT"])
    content_types_accepted do: ["application/json": :from_json]

    defh from_json do
      data =
        MyServer.Riak.search("rbessonnier_orders_index", "remoteid:#{state.name}", 0, 30)
        |> Enum.map(&Map.get(&1, "key"))

      {:ok, pid} = MyServer.FSMSupervisor.start_child(hd(data))
      MyServer.Payment.update_order(pid, hd(data))
    end
  end

  resource "/*_" do
    %{}
  after
    content_types_provided(do: ["text/html": :to_content])

    defh to_content(conn, state) do
      render =
        Reaxt.render!(
          :app,
          %{path: conn.request_path, cookies: conn.cookies, query: conn.params},
          30_000
        )

      Server.Router.layout(render)
    end

    defp default_plain("application/octet-stream"), do: "text/html"
    defp default_plain(type), do: type
  end
end
