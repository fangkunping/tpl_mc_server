defmodule MiddleController.Action.WebsocketAction do
  @behaviour MiddleController.Interface.IWebsocketAction
  alias MiddleController.Manager.WebSocketManager
  alias MiddleController.Manager.CacheManager
  use MiddleController.Const.HttpCode

  # 心跳
  def run(socket, "HB") do
    WebSocketManager.send_msg(socket, "echo: HB")
  end

  def run(socket, bin_string) do
    WebSocketManager.send_msg(socket, "echo: #{bin_string}")
  end

  @doc """
    握手过程， 用于判断用户能否登录
    request 和 add_socket 一样
  """
  def do_hand_shake(_request) do
    true
  end

  @doc """
    ws://127.0.0.1:1000/this_is_request
    request 就是： "/this_is_request"
  """
  def add_socket(_request, socket) do
    websocket_list =
      case CacheManager.fetch(:websocket) do
        nil -> []
        d -> d
      end

    CacheManager.store(:websocket, [socket | websocket_list])
  end

  def remove_socket(socket) do
    websocket_list =
      case CacheManager.fetch(:websocket) do
        nil -> []
        d -> d
      end

    new_websocket_list =
      websocket_list
      |> Enum.reduce(
        [],
        fn
          ^socket, acc -> acc
          sk, acc -> [sk | acc]
        end
      )

    CacheManager.store(:websocket, new_websocket_list)
  end
end
