# package apps.middle_controller.lib.manager;
defmodule MiddleController.Manager.WebSocketManager do
  use GenServer
  alias MiddleController.Manager.WebSocketManager, as: State
  alias MiddleController.Action.WebsocketAction
  defstruct [:port, :listen]

  # Client

  def start_link(default_state) do
    GenServer.start_link(__MODULE__, default_state, [{:name, __MODULE__}])
  end

  # 发送信息
  def send_msg(socket, msg, set_active? \\ false) do
    send_to(socket, :socket_server_ws.encode(msg))

    if set_active? do
      active(socket)
    end
  end

  def send_msg_by(msg, socket, set_active? \\ false) do
    send_msg(socket, msg, set_active?)
  end

  defp send_to(socket, message) do
    case MiddleController.Tools.ConfigUtil.use_wss?() do
      true ->
        :ssl.send(socket, message)

      false ->
        :gen_tcp.send(socket, message)
    end
  end

  # 激活状态
  def active(socket) do
    :ssl.setopts(socket, [{:active, :once}])
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
    # IO.inspect(bin)

    WebsocketAction.run(socket, KunERAUQS.D0_f.json_decode(bin))
    # send_msg(socket, bin, true)
    active(socket)
  end

  # 信息次序 2
  # websocket 请求时获得的 链接信息
  def socket_data_in(:hand_shake, socket, socket_pid, {method, uri, version}) do
    GenServer.call(__MODULE__, {:hand_shake, socket, socket_pid, {method, uri, version}})
  end

  # Server (callbacks)

  def init(state) do
    websocket_server_conf = Application.get_env(:middle_controller, :websocket_server_conf)
    case MiddleController.Tools.ConfigUtil.use_wss?() do
      true ->
        :socket_server_wss.start(
          websocket_server_conf.port,
          &socket_data_in/4,
          __MODULE__,
          websocket_server_conf.pre_start_process
        )

      false ->
        :socket_server_ws.start(
          websocket_server_conf.port,
          &socket_data_in/4,
          __MODULE__,
          websocket_server_conf.pre_start_process
        )
    end

    {:ok, state}
  end

  # 握手信息
  def handle_call(
        {:hand_shake, socket, _socket_pid, {_method, {:abs_path, request}, _version}},
        _form,
        state
      ) do
    IO.inspect(request)

    case WebsocketAction.do_hand_shake(request) do
      true ->
        WebsocketAction.add_socket(request, socket)
        {:reply, true, state}

      _ ->
        {:reply, false, state}
    end
  end

  def handle_call({:hand_shake, _, _, _}, _form, state) do
    {:reply, false, state}
  end

  def handle_call(_, _form, state) do
    {:noreply, state}
  end

  # 信息次序 1
  def handle_info({:msg, 'Socket connected', _socket, _pid}, state) do
    # :io.format("Socket connected: ~p ~p~n", [socket, pid])
    {:noreply, state}
  end

  # 信息次序 4
  def handle_info({:msg, 'socket closed', socket}, state) do
    # :io.format("socket closed: ~p~n", [socket])
    WebsocketAction.remove_socket(socket)
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
