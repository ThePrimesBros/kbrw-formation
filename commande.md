Chap 1 - 
iex -S mix
{:ok, database_pid} = MyServer.Database.start_link()
JsonLoader.load_to_database(database_pid, "./Resources/chap1/orders_dump/orders_chunk0.json")
MyServer.Database.add_key_value(database_pid, :key, 42)
MyServer.Database.search(database_pid, [{"key", 42}])
  orders = [%{"id" => "toto", "key" => 42},%{"id" => "test", "key" => "42"},%{"id" => "tata", "key" => "Apero?},%{"id" => "kbrw", "key" => "Oh yeah"},]

Chap 2 -
mix run --no-halt