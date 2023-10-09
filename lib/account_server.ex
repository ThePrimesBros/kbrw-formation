defmodule AccountServer do
  def handle_cast({:credit, c}, amount), do: amount + c
  def handle_cast({:debit, c}, amount), do: amount - c
  def handle_call(:get, amount), do: amount

  def start_link(initial_amount) do
    MyGenericServer.start_link(__MODULE__, initial_amount)
  end
end

# {:ok, my_account} = AccountServer.start_link(4)
# MyGenericServer.cast(my_account, {:credit, 5})
# MyGenericServer.cast(my_account, {:credit, 2})
# MyGenericServer.cast(my_account, {:debit, 3})
# amount = MyGenericServer.call(my_account, :get)
# IO.puts "current credit hold is #{amount}"
