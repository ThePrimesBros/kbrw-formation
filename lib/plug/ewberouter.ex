defmodule MyJSONApi do
  use Ewebmachine.Builder.Handlers
  plug(:cors)
  plug(:add_handlers, init: %{})

  content_types_provided(do: ["application/json": :to_json])
  defh(to_json, do: Poison.encode!(state[:json_obj]))

  defp cors(conn, _), do: put_resp_header(conn, "Access-Control-Allow-Origin", "*")
end

defmodule ErrorRoutes do
  use Ewebmachine.Builder.Resources
  resources_plugs

  resource "/error/:status" do
    %{s: elem(Integer.parse(status), 0)}
  after
    content_types_provided(do: ["text/html": :to_html, "application/json": :to_json])
    defh(to_html, do: "<h1> Error ! : '#{Ewebmachine.Core.Utils.http_label(state.s)}'</h1>")

    defh(to_json,
      do: ~s/{"error": #{state.s}, "label": "#{Ewebmachine.Core.Utils.http_label(state.s)}"}/
    )

    finish_request(do: {:halt, state.s})
  end
end

defmodule Server.EwebRouter do
  use Ewebmachine.Builder.Resources
  if Mix.env() == :dev, do: plug(Ewebmachine.Plug.Debug)
  resources_plugs(error_forwarding: "/error/:status", nomatch_404: true)
  plug(ErrorRoutes)

  resource "/hello/:name" do
    %{name: name}
  after
    content_types_provided(do: ["application/xml": :to_xml])
    defh(to_xml, do: "<Person><name>#{state.name}</name>")
  end

  resource "/hello/json/:name" do
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

  resource "/api/orders" do
    %{}
  after
    plug :fetch_query_params
    plug MyJSONApi

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

  resource "/static/*path" do
    %{path: Enum.join(path, "/")}
  after
    resource_exists(do: File.regular?(path(state.path)))
    content_types_provided(do: [{state.path |> Plug.MIME.path() |> default_plain, :to_content}])
    defh(to_content, do: File.stream!(path(state.path), [], 300_000_000))
    defp path(relative), do: "#{:code.priv_dir(:ewebmachine_example)}/web/#{relative}"
    defp default_plain("application/octet-stream"), do: "text/plain"
    defp default_plain(type), do: type
  end
end
