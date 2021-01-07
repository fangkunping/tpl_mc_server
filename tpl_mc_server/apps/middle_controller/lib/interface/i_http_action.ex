defmodule MiddleController.Interface.IHttpAction do
  @callback run_post(
              path :: String.t(),
              post_data :: String.t() | %{String.t() => String.t()},
              header :: [{atom, charlist}, ...]
            ) :: any

  @callback run_get(
              path :: String.t(),
              query_data :: String.t() | %{String.t() => String.t()},
              header :: [{atom, charlist}, ...]
            ) :: any
end
