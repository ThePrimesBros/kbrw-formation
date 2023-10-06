defmodule AccountServer do
  def handle_cast({:credit, c}, amount), do: amount + c
  def handle_cast({:debit, c}, amount), do: amount - c
  def handle_call(:get, amount), do: amount

  def start_link(initial_amount) do
    MyGenericServer.start_link(__MODULE__, initial_amount)
  end
end
