defmodule MiddleController.Interface.IWebsocketAction do
  @callback run(socket :: term, bin_string :: String.t()) :: any
  @callback do_hand_shake(request :: String.t()) :: true | false
  @callback add_socket(request :: String.t(), socket :: term) :: any
  @callback remove_socket(socket :: term) :: any
end
