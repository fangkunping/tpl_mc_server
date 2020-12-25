defmodule MiddleController.Action.HttpAction do
  @behaviour MiddleController.Interface.IHttpAction
  alias MiddleController.Tool.WebUtil
  alias MiddleController.Manager.CacheManager

  use MiddleController.Const.HttpCode

  def run_post("/get", post_data, _header) do
    WebUtil.response_ok(CacheManager.fetch(post_data["key"]))
    # (post_data.username, post_data.password)
  end

  def run_post("/set", post_data, _header) do
    CacheManager.store(post_data["key"], post_data["value"])
    WebUtil.response_ok()
  end

  def run_post(_, _, _) do
    WebUtil.unknow_error_response_data()
  end

  def run_get("/get", query_data, _header) do
    WebUtil.response_ok(CacheManager.fetch(query_data["key"]))
  end

  def run_get("/set", query_data, _header) do
    CacheManager.store(query_data["key"], query_data["value"])
    WebUtil.response_ok()
  end

  def run_get(_, _, _) do
    WebUtil.unknow_error_response_data()
  end
end
