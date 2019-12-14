defmodule Dapi.HttpSocketManager do
  @header_common "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *"
  @content_type_text 'text/html'
  @content_type_form_urlencode 'application/x-www-form-urlencoded'
  @content_type_json 'application/json'
  # Client

  def init_self() do
    # 获取配置文件
    websocket_server_conf = Application.get_env(:dapi, :http_socket_server)
    # 启动 http
    :socket_server_http.start(
      websocket_server_conf.port,
      &socket_data_in/4,
      nil,
      websocket_server_conf.pre_start_process
    )
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
    # IO.inspect({:post, path, post_body, headers})
    # IO.inspect(get_content_type(headers) |> decode(post_body))
    # 获取 来源的ip和端口
    # IO.inspect(:inet.peername(socket))
    from_ipv4 = get_from_ipv4(:inet.peername(socket))

    result =
      Dapi.HttpAction.run(
        headers,
        path,
        from_ipv4,
        get_content_type(headers) |> decode(post_body)
      )

    send_to(socket, result, @content_type_json)
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

  # 用于ab 测试
  def decode(nil, data) do
    URI.decode_query(data)
  end

  def decode(_, data) do
    data
  end

  # 获取 来源ip
  def get_from_ipv4({:ok, {{p1, p2, p3, p4} = ip_address, port_number}}) do
    {"#{p1}.#{p2}.#{p3}.#{p4}", port_number, ip_address}
  end

  def get_from_ipv4(_) do
    {"0.0.0.0", 0, {0, 0, 0, 0}}
  end
end
