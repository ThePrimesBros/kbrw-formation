defmodule MyServer.DatabaseTest do
  use ExUnit.Case, async: true

  # Start the supervisor for each test
  setup do
    {:ok, _} = Supervisor.start_link(MyServer.ServSupervisor)
    {:ok, pid} = MyServer.Database.start_link([])
    {:ok, pid: pid}
  end

  test "add, get, update, and delete values" do
    pid = self()

    MyServer.Database.add_key_value(@pid, :key1, "value1")
    assert MyServer.Database.get_value(@pid, :key1) == "value1"

    MyServer.Database.update_value(@pid, :key1, "new_value")
    assert MyServer.Database.get_value(@pid, :key1) == "new_value"

    MyServer.Database.delete_key(@pid, :key1)
    assert MyServer.Database.get_value(@pid, :key1) == nil
  end
end
