defmodule Dapi.HttpSocketManager do
  use GenServer
  alias Dapi.HttpSocketManager, as: State
  defstruct [:port, :listen]

  @header_common "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *"
  @content_type_text 'text/html'
  @content_type_form_urlencode 'application/x-www-form-urlencoded'
  @content_type_json 'application/json'
  # Client

  def start_link(default_state) do
    GenServer.start_link(__MODULE__, default_state, [{:name, __MODULE__}])
  end

  # 发送信息
  def send_to(socket, msg, header \\ @content_type_text) do
    :gen_tcp.send(
      socket,
      "#{@header_common}\r\nContent-Type: #{header}; charset=utf-8\r\nContent-Length: #{
        :erlang.size(msg)
      }\r\n\r\n#{msg}"
    )

    :gen_tcp.close(socket)
  end

  def send_by(msg, socket, header \\ @content_type_text) do
    send_to(socket, msg, header)
  end

  # http 来的 数据
  def socket_data_in(:data_in, socket, _socket_pid, {mode, path, value, headers}) do
    socket_data_in(socket, {mode, List.to_string(path), List.to_string(value), headers})
  end

  # post 数据
  def socket_data_in(socket, {:post, path, post_body, headers}) do
    IO.inspect({:post, path, post_body, headers})
    IO.inspect(get_content_type(headers) |> decode(post_body))
    # 获取 来源的ip和端口
    IO.inspect(:inet.peername(socket))
    send_to(socket, KunERAUQS.D0_f.json_encode(%{code: 0, data: "ok"}), @content_type_json)
  end

  # get 数据
  def socket_data_in(socket, {:get, path, value, headers}) do
    IO.inspect({:get, path, value, headers})
    IO.inspect(get_content_type(headers) |> decode(value))
    # 获取 来源的ip和端口
    IO.inspect(:inet.peername(socket))
    send_to(socket, KunERAUQS.D0_f.json_encode(%{code: 0, data: "ok"}), @content_type_json)
  end

  # 获取Content-Type类型
  def get_content_type(headers) do
    headers
    |> Enum.reduce_while(nil, fn
      {:"Content-Type", v}, _ -> {:halt, v}
      _, acc -> {:cont, acc}
    end)
  end

  # 解码来源数据
  def decode(@content_type_json, data) do
    KunERAUQS.D0_f.json_decode(data)
  end

  def decode(@content_type_form_urlencode, data) do
    URI.decode_query(data)
  end

  def decode(_, data) do
    data
  end
  # Server (callbacks)

  def init(state) do
    # 需要修改配置文件相关信息
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
