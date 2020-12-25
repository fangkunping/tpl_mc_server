defmodule MiddleController.Const.HttpCode do
  defmacro __using__(_params) do
    quote do
      @code_unknown_error 44444
      @incorrect_loginname_password 4101
      @sp_token_fail 4102

      @http_time_out 1000_000

      @command_heartbeat 1
      @command_sp_api 11
      @command_notice_api 12
      @command_echo_api 13
      @command_chat_api 14
    end
  end
end
