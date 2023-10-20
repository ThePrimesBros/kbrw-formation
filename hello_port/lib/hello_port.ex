defmodule TutoKBRWStack do
  use GenServer
  @moduledoc """
    Generic port as gen_server wrapper :
    send a message at init, first message is remote initial state
    cast and call encode and decode erlang binary format
  """
  def init(_) do
    port = Port.open({:spawn,'node hello.js'}, [:binary,:exit_status, packet: 4])
    send(port,{self,{:command,:erlang.term_to_binary(0)}})
    {:ok,port}
  end
  def start_link(opt) do
    GenServer.start_link(__MODULE__, opt, name: __MODULE__)
  end

  def handle_info({port,{:exit_status,0}},port), do: {:stop,:normal,port}
  def handle_info({port,{:exit_status,_}},port), do: {:stop,:port_terminated,port}
  def handle_info(_,port), do: {:noreply,port}
  
  def handle_cast(term,port) do
    send(port,{self,{:command,:erlang.term_to_binary(term)}})
    {:noreply,port}
  end
  def handle_call(term,_reply_to,port) do
    send(port,{self,{:command,:erlang.term_to_binary(term)}})
    res = receive do {^port,{:data,b}}->:erlang.binary_to_term(b) end
    {:reply,res,port}
  end

  def call(req) do
    GenServer.call(__MODULE__, req)
  end
  def cast(req) do
    GenServer.cast(__MODULE__, req)    
  end
end
