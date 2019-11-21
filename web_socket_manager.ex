defmodule GuaJi.WebSocketManager do
  use GenServer
  alias GuaJi.WebSocketManager, as: State
  defstruct [:port, :listen]

  # Client

  def start_link(default_state) do
    GenServer.start_link(__MODULE__, default_state, [{:name, __MODULE__}])
  end

  # 发送信息
  def send(socket, msg, set_active? \\ false) do
    :gen_tcp.send(socket, :socket_server_ws.encode(msg))
    if set_active? do
      active(socket)
    end
  end

  def send_by(msg, socket, set_active? \\ false) do
    :gen_tcp.send(socket, :socket_server_ws.encode(msg))
    if set_active? do
      active(socket)
    end
  end

  # 激活状态
  def active(socket) do
    :inet.setopts(socket, [{:active, :once}])
  end

  def close(socket) do
    socket |> :gen_tcp.close()
  end

  def close_and_delete(socket) do
    socket |> close
  end

  # 信息次序 3
  # websocket 请求的数据
  def socket_data_in(:data_in, socket, _socket_pid, bin) do

  end

  # 信息次序 2
  # websocket 请求时获得的 链接信息
  def socket_data_in(:hand_shake, socket, socket_pid, {method, uri, version}) do
    GenServer.call(__MODULE__, {:hand_shake, socket, socket_pid, {method, uri, version}})
  end

  # Server (callbacks)

  def init(state) do
    websocket_server_conf = Application.get_env(:gua_ji, :websocket_server)

    :socket_server_ws.start(
      websocket_server_conf.port,
      &socket_data_in/4,
      __MODULE__,
      websocket_server_conf.pre_start_process
    )

    {:ok, state}
  end

  # 握手信息
  def handle_call(
        {:hand_shake, socket, _socket_pid, {_method, {:abs_path, request} = uri, _version}},
        _form,
        state
      ) do
    result = true
    {:reply, result, state}
  end

  def handle_call({:hand_shake, _, _, _}, _form, state) do
    {:reply, false, state}
  end

  def handle_call(_, _form, state) do
    {:noreply, state}
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
