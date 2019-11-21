defmodule Dapi.HttpSocketManager do
  use GenServer
  alias Dapi.HttpSocketManager, as: State
  defstruct [:port, :listen]

  @header_common "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *"
  @content_type_text "text/html"
  @content_type_json "application/json"
  @content_type_jsonp "application/javascript"
  # Client

  def start_link(default_state) do
    GenServer.start_link(__MODULE__, default_state, [{:name, __MODULE__}])
  end

  # 发送信息
  def send_to(socket, msg, header \\ @content_type_text) do
    :gen_tcp.send(
      socket,
      "#{@header_common}\r\nContent-Type: #{header}; charset=utf-8\r\nContent-Length: #{:erlang.size(msg)}\r\n\r\n#{msg}"
    )
    :gen_tcp.close(socket)
  end

  def send_by(msg, socket, header \\ @content_type_text) do
    send_to(socket, msg, header)
  end

  # http 来的 post 数据
  def socket_data_in(:data_in, socket, _socket_pid, {:post, path, post_body}) do
    IO.inspect({:post, path, post_body})
    send_to(socket, KunERAUQS.D0_f.json_encode(%{code: 0, data: "ok"}), @content_type_json)
  end

  # http 来的 get 数据
  def socket_data_in(:data_in, socket, _socket_pid, {:get, path, value}) do
    IO.inspect({:get, path, value})
    send_to(socket, KunERAUQS.D0_f.json_encode(%{code: 0, data: "ok"}), @content_type_json)
  end

  # Server (callbacks)

  def init(state) do
    websocket_server_conf = Application.get_env(:dapi, :http_socket_server)

    :socket_server_http.start(
      websocket_server_conf.port,
      &socket_data_in/4,
      __MODULE__,
      websocket_server_conf.pre_start_process
    )
    {:ok, state}
  end

  # 信息次序 1
  def handle_info({:msg, 'Socket connected', socket, pid}, state) do
    :io.format("Socket connected: ~p ~p~n", [socket, pid])
    {:noreply, state}
  end

  # 信息次序 4
  def handle_info({:msg, 'socket closed', socket}, state) do
    :io.format("socket closed: ~p~n", [socket])
    {:noreply, state}
  end

  # 信息次序 1
  def handle_info({:error, 'Socket connect error', reason}, state) do
    :io.format("Socket connect error: ~p~n", [reason])
    {:noreply, state}
  end

  # 信息次序 0
  def handle_info({:ok, 'Listen ok', port, listen}, _) do
    :io.format("Listen ok: ~p ~p~n", [port, listen])
    {:noreply, %State{port: port, listen: listen}}
  end

  # 信息次序 0
  def handle_info({:error, 'Listen error', reason}, state) do
    :io.format("Listen error: ~p~n", [reason])
    {:noreply, state}
  end

  def handle_info(x, state) do
    :io.format('unknown_message:~p~n', [x])
    {:noreply, state}
  end
end
