defmodule MyServer.Riak do
  def url, do: "https://kbrw-sb-tutoex-riak-gateway.kbrw.fr"


  def auth_header do
    username = "sophomore"
    password = "jlessthan3tutoex"
    auth = :base64.encode_to_string("#{username}:#{password}")
    [{'authorization', 'Basic #{auth}'}]
  end

  def test do
    {:ok, {{_, 200, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/buckets?buckets=true', MyServer.Riak.auth_header()},
        [],
        []
      )
  end

  def add_object(object) do
    IO.inspect(object)
    {:ok, {{_, 201, _message}, _headers, body}} =
      :httpc.request(
        :post,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys?returnbody=true',
         MyServer.Riak.auth_header(), 'application/json', object},
        [],
        []
      )

    case Poison.decode(body) do
      {:ok, decoded_object} ->
        {:ok, decoded_object}

      {:error, _} ->
        {:error, "Failed to decode the response body as JSON"}
    end
  end

  def get_buckets() do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/buckets?buckets=true', MyServer.Riak.auth_header()},
        [],
        []
      )

    Poison.decode!(body)["buckets"]
  end

  def get_keys() do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys?keys=true',
         MyServer.Riak.auth_header()},
        [],
        []
      )

    Poison.decode!(body)["keys"]
  end

  def delete_key(key) do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :delete,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys/#{key}',
         MyServer.Riak.auth_header()},
        [],
        []
      )

    :ok
  end

  def get(key) do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys/#{key}',
         MyServer.Riak.auth_header()},
        [],
        []
      )

      Poison.decode!(body)
  end

  def update_key(key, value) do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :put,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys/#{key}',
         MyServer.Riak.auth_header(), 'application/json', value},
        [],
        []
      )

    :ok
  end

  ##########################################################################
  #                     SCHEMA                                             #
  ##########################################################################

  # UPLOAD Schema
  def define_schema(schema) do
    {:ok, content} = File.read(schema)

    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :put,
        {'#{MyServer.Riak.url()}/search/schema/rbessonnier_orders_schema',
         MyServer.Riak.auth_header(), 'application/xml', content},
        [],
        []
      )
    IO.inspect(_code)
    :ok
  end

  # Create Index
  def create_index(schema) do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :put,
        {'#{MyServer.Riak.url()}/search/index/rbessonnier_orders_index',
         MyServer.Riak.auth_header(), 'application/json', '{"schema": "#{schema}"}'},
        [],
        []
      )
      IO.inspect(_code)
    :ok
  end

  # Asssign Index
  def assign_index() do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :put,
        {'#{MyServer.Riak.url()}/buckets/bessonnier_order/props', MyServer.Riak.auth_header(),
         'application/json', '{"props":{"search_index": "rbessonnier_orders_index"}}'},
        [],
        []
      )
      IO.inspect(_code)
    :ok
  end

  # Get Index
  def get_index() do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/search/index', MyServer.Riak.auth_header()},
        [],
        []
      )

    body
  end

  # Empty bucket

  def empty_bucket() do
    res = get_keys()

    for n <- res do
      IO.inspect(n)

      {:ok, {{_, _code, _message}, _headers, body}} =
        :httpc.request(
          :delete,
          {'#{MyServer.Riak.url()}/buckets/bessonnier_order/keys/#{n}',
           MyServer.Riak.auth_header()},
          [],
          []
        )
    end

    :ok
  end

  # Delete Bucket
  def delete_bucket() do
    res = empty_bucket()

    case empty_bucket() do
      :ok ->
        {:ok, {{_, _code, _message}, _headers, body}} =
          :httpc.request(
            :delete,
            {'#{MyServer.Riak.url()}/buckets/bessonnier_order/props',
             MyServer.Riak.auth_header()},
            [],
            []
          )

        :ok
    end
  end

  # Reindex bucket

  ##########################################################################
  #                               RESEARCH                                 #
  ##########################################################################

  def search(index, query, page \\ 0, rows \\ 30, sort \\ "remoteid") do
    {:ok, {{_, _code, _message}, _headers, body}} =
      :httpc.request(
        :get,
        {'#{MyServer.Riak.url()}/search/query/#{index}/?wt=json&q=#{query}&start=#{page*rows}&rows=#{rows}',
         MyServer.Riak.auth_header()},
        [],
        []
      )

    Poison.decode!(body)["response"]["docs"]
    |> Enum.map(fn res -> %{"bucket" => res["_yz_rb"], "key" => res["_yz_rk"]} end)
  end
end
