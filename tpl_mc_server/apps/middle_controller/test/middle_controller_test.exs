defmodule MiddleControllerTest do
  use ExUnit.Case
  doctest MiddleController

  test "greets the world" do
    assert MiddleController.hello() == :world
  end
end
