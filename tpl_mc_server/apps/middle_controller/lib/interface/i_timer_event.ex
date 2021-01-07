defmodule MiddleController.Interface.ITimerEvent do
  @callback new() :: any

  @callback tick(any) :: any
end
