defmodule Lab5Test do
  use ExUnit.Case
  doctest Lab5

  test "greets the world" do
    assert Lab5.hello() == :world
  end
end
