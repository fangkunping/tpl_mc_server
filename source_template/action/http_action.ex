defmodule MiddleController.Action.HttpAction do
  import Kunerauqs.CommonTools, only: [then: 3]
  alias MiddleController.Action.HttpAction.LoginProcess, as: LoginProcess
  alias MiddleController.Tools.WebUtil, as: WebUtil
  alias MiddleController.Worker.TokenWorker, as: TokenWorker
  alias MiddleController.Worker.ResponseCacheWorker
  alias MiddleController.Action.HttpAction.SpapiProcess

  use MiddleController.Const.HttpCode

  import Kunerauqs.ROP

  def run("/login", post_data, _header) do
    login_name = post_data["LoginName"]
    password = post_data["Password"]

    {:ok, password_hash} =
      WebUtil.get_logic_server_result(%{
        c: "user",
        a: "hash_password",
        password: password
      })

    # 检查用户名密码是否正确
    LoginProcess.do_check_user_login_info(login_name, password_hash)
    |> then(
      # 正确
      fn response_data ->
        user_id = response_data["ID"]

        TokenWorker.find_token(user_id)
        |> then(
          # 已经登录，返回 hash id
          fn token -> WebUtil.response_ok(token) end,
          # 未登录，进行登录操作
          fn _ ->
            LoginProcess.do_user_login(login_name, password_hash) >>>
              try_catch(LoginProcess.do_create_token(user_id))
            |> then(
              # 返回 hash id
              fn hash_id -> WebUtil.response_ok(hash_id) end,
              # 未知错误
              fn _ -> WebUtil.unknow_error_response_data() end
            )
          end
        )
      end,
      # 用户名密码错误
      fn _ -> WebUtil.response_error(@incorrect_loginname_password) end
    )

    # (post_data.username, post_data.password)
  end

  def run("/logout", _post_data, header) do
    token = header |> List.keyfind('Token', 0) |> elem(1) |> List.to_string()

    TokenWorker.delete_token_data(token)
    WebUtil.response_ok()
  end

  def run("/saveHeadPicture", post_data, _header) do
    token = post_data["token"]
    img_base64 = post_data["base64"]
    user_info = SpapiProcess.get_user_info(token)
    neb_conf = Application.get_env(:middle_controller, :neb_conf)
    user_id = Map.get(user_info, "ID")
    {:ok, data} = Base.decode64(img_base64)
    File.write("#{neb_conf.head_picture}#{user_id}.png", data, [:binary])

    WebUtil.response_ok()
  end

  def run("/reset_password", post_data, _header) do
    email = post_data["email"]
    WebUtil.get_logic_server_result(%{
      c: "user",
      a: "forgot_password",
      email: email
    })

    WebUtil.response_ok()
  end

  def run("/tttt", post_data, _header) do
    api_uri = post_data["uri"]
    login_token = post_data["LoginToken"]

    new_postdata =
      post_data
      |> Map.delete("uri")
      |> Map.delete("LoginToken")

    self_pid = self()

    ResponseCacheWorker.sp_request(api_uri, login_token, new_postdata, fn result ->
      result
      |> then(
        fn response ->
          send(self_pid, {:response, response})
          response
        end,
        nil
      )
    end)

    receive do
      {:response, response} ->
        response
    end
  end

  def run(_, _, _) do
    WebUtil.unknow_error_response_data()
  end
end
