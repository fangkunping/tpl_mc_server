defmodule MiddleController.Action.HttpAction do
  @behaviour MiddleController.Interface.IHttpAction
  alias MiddleController.Tool.WebUtil
  import Kunerauqs.CommonTools

  use MiddleController.Const.HttpCode

  def run_get(path, query_data, header) do
    [&MiddleController.Action.TestCacheAction.run_get/3]
    |> call_other_action(path, query_data, header)
  end

  def run_post(path, post_data, header) do
    [&MiddleController.Action.TestCacheAction.run_post/3]
    |> call_other_action(path, post_data, header)
  end

  defp call_other_action(actions, path, data, header) do
    actions
    |> Enum.reduce_while({:error, nil}, fn f, old_result ->
      case f.(path, data, header) do
        nil -> {:cont, old_result}
        other -> {:halt, {:ok, other}}
      end
    end)
    |> then(
      fn response -> response end,
      fn _ ->
        WebUtil.unknow_error_response_data()
      end
    )
  end
end
