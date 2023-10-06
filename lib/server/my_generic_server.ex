defmodule MyGenericServer do
  # Function to start the server process
  def start_link(callback_module, server_initial_state) do
    #IO.inspect(spawn_link(__MODULE__, :loop, [{callback_module, server_state}]))
    pid = spawn_link(fn -> loop({callback_module, server_initial_state}) end)
    {:ok, pid}
  end

  # Main loop function
  def loop({callback_module, state}) do
    receive do
      {:cast, request} ->
        new_state = callback_module.handle_cast(request, state)
        loop({callback_module, new_state})

      {:call, from, request} ->
        response = callback_module.handle_call(request, state)
        send(from, {:reply, response})
        loop({callback_module, state})
    end
  end


  # Wrapper for cast operations
  def cast(pid, request) do
    send(pid, {:cast, request})
  end

  # Wrapper for call operations
  def call(pid, request) do
    self = self()
    send(pid, {:call, self, request})
    receive do
      {:reply, response} -> response
    end
  end
end
