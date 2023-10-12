defmodule Server.TheCreator do
  @doc false

  # Initialize the @routes module attribute as an empty list

  defmacro __using__(_opts) do
    quote do
      import Server.TheCreator
      @routes []
      @before_compile Server.TheCreator
    end
  end

  defmacro my_get(path, do: block) do
    quote do
      @routes [{unquote(path), unquote(block)} | @routes]
    end
  end

  defmacro my_error(code: error_code, content: error_content) do
    quote do
      @error_code unquote(error_code)
      @error_content unquote(error_content)
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do

      def init(_opts) do
        {:ok, %{}}
      end

      defp find_route(path) do
        Enum.find_value(@routes, fn {route_path, route_block} ->
          if path == route_path do
            {200, route_block}
          end
        end)
      end

      def call(conn, _opts) do
        path = conn.request_path
        case find_route(path) do
          {200, block} ->
            {_, string} = block
            conn
            |> Plug.Conn.put_status(200)
            |> Plug.Conn.send_resp(:ok, string)

          _ ->
            conn
            |> Plug.Conn.put_status(@error_code)
            |> Plug.Conn.send_resp(:not_found, @error_content)
        end
      end
    end
  end
end
