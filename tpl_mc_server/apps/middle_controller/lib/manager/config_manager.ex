defmodule MiddleController.Manager.ConfigManager do
  defp get_conf(key, value) do
    conf = Application.get_env(:middle_controller, key)
    Map.get(conf, value)
  end

  def websocket_server_conf(key) do
    get_conf(:websocket_server_conf, key)
  end

  def http_server_conf(key) do
    get_conf(:http_server_conf, key)
  end

  def socket_server_conf(key) do
    get_conf(:socket_server_conf, key)
  end
end
