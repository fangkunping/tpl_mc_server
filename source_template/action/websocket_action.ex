defmodule MiddleController.Action.WebsocketAction do
  alias MiddleController.Tools.WebUtil, as: WebUtil
  alias MiddleController.Worker.TokenWorker
  alias MiddleController.Action.HttpAction.SpapiProcess
  import Kunerauqs.CommonTools, only: [then: 3, then_ok: 2]
  use MiddleController.Const.HttpCode

  def run(socket, %{
        "command" => @command_sp_api,
        "token" => token,
        "uuid" => uuid,
        "data" => %{
          "api" => api,
          "postdata" => postdata
        }
      }) do
    # {api, postdata} |> IO.inspect()
    do_action(token, socket, api, uuid, postdata)
  end

  # 心跳
  def run(socket, %{
        "command" => @command_heartbeat
      }) do
    SpapiProcess.send_msg(socket, %{command: @command_heartbeat})
  end

  def do_action(token, socket, "/websocket/getUserInfo", uuid, _postdata) do
    case SpapiProcess.get_user_info(token) do
      nil ->
        SpapiProcess.response_sp_token_fail(socket, uuid)

      user_info ->
        SpapiProcess.send_msg(socket, %{
          command: @command_sp_api,
          uuid: uuid,
          data: user_info |> Map.put("Password", nil)
        })
    end
  end

  def do_action(_token, socket, "/Xampp", uuid, postdata) do
    WebUtil.create_chat_api_uri()
    |> WebUtil.get_http(:post, %{
      header: [],
      body: postdata
    })
    |> then(
      fn response ->
        SpapiProcess.send_msg(socket, %{
          command: @command_sp_api,
          uuid: uuid,
          data: response
        })

        # 如果是chat 则通知前端刷新数据
        SpapiProcess.xampp_echo(postdata)
      end,
      nil
    )
  end

  def do_action(token, socket, "/Notice/T_Insert" = api, uuid, postdata) do
    SpapiProcess.send_to_sharepoint(
      token,
      socket,
      api,
      uuid,
      postdata,
      nil,
      fn ->
        from_userid = postdata["Data"]["FkFromUserId"]
        to_userid = postdata["Data"]["FkToUserId"]
        # 找到用户的socket
        [from_userid, to_userid]
        |> Enum.each(fn
          0 ->
            nil

          "0" ->
            nil

          user_id ->
            # 向参与的用户发信息
            TokenWorker.find_sockets(user_id)
            |> then_ok(fn sockets ->
              sockets
              |> Enum.each(fn socket ->
                SpapiProcess.send_msg(socket, %{
                  command: @command_notice_api,
                  action: "reload"
                })
              end)
            end)
        end)
      end
    )
  end

  def do_action(token, socket, "/User/Update" = api, uuid, or_postdata) do
    {:ok, postdata} =
      WebUtil.get_logic_server_result(%{
        c: "user",
        a: "hash_postdata",
        post_data: KunERAUQS.D0_f.json_encode(or_postdata)
      })

    SpapiProcess.send_to_sharepoint(
      token,
      socket,
      api,
      uuid,
      postdata,
      nil,
      fn ->
        # 修改用户数据后，
        # 如果用户的类型改变，那么
        # 清除掉用户缓存，让用户重新登录
        modify_user_id = postdata["Data"]["ID"]
        modify_user_info = SpapiProcess.get_user_info(modify_user_id)

        (postdata["Data"]["FkUserGroupId"] != modify_user_info["FkUserGroupId"])
        |> Kunerauqs.CommonTools.bool_to_then()
        |> then_ok(fn _ ->
          TokenWorker.delete_token_data(modify_user_id)
        end)
      end
    )
  end

  def do_action(token, socket, "/User/UpdateSome" = api, uuid, or_postdata) do
    {:ok, postdata} =
      WebUtil.get_logic_server_result(%{
        c: "user",
        a: "hash_postdata",
        post_data: KunERAUQS.D0_f.json_encode(or_postdata)
      })

    SpapiProcess.send_to_sharepoint(
      token,
      socket,
      api,
      uuid,
      postdata,
      fn ->
        user_info = SpapiProcess.get_user_info(token)
        # 修改用户数据后，重新读取用户数据到缓存中
        SpapiProcess.reload_login_user_info(token, user_info)
      end
    )
  end

  def do_action(token, socket, "/User/Add" = api, uuid, or_postdata) do
    {:ok, postdata} =
      WebUtil.get_logic_server_result(%{
        c: "user",
        a: "hash_postdata",
        post_data: KunERAUQS.D0_f.json_encode(or_postdata)
      })

    SpapiProcess.send_to_sharepoint(
      token,
      socket,
      api,
      uuid,
      postdata
    )
  end

  def do_action(token, socket, api, uuid, postdata) do
    SpapiProcess.send_to_sharepoint(
      token,
      socket,
      api,
      uuid,
      postdata
    )
  end

  def do_hand_shake(request) do
    token = get_shake_token(request)

    # token 是否存在
    TokenWorker.get_token_data(token)
    |> then(
      fn _ -> true end,
      fn _ -> false end
    )
  end

  def add_socket(request, socket) do
    TokenWorker.add_socket(get_shake_token(request), socket)
  end

  def remove_socket(socket) do
    TokenWorker.remove_socket(socket)
  end

  defp get_shake_token(request) do
    request
    |> List.to_string()
    |> String.slice(1, 1500)
  end
end
