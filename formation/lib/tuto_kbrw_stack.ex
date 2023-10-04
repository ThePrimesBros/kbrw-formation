defmodule TutoKbrwStack do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    load_data_on_start()

    children = [
      {MyServer.Database, []}  # Define your child processes here
    ]

    # Start the supervisor
    opts = [strategy: :one_for_one, name: TutoKbrwStack.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def load_data_on_start() do
    my_kv_db = %{}  # Create your database here
    json_file_path = "../../Ressources/chap1/orders_dump/orders_chunks0.json"  # Adjust the path to your JSON file

    # Load JSON data into the database
    JsonLoader.load_to_database(my_kv_db, json_file_path)

    # You can return the database or do other operations here
  end
end
