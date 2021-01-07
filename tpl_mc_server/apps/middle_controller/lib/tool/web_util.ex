defmodule MiddleController.Tool.WebUtil do
  use MiddleController.Const.HttpCode
  import Kunerauqs.CommonTools, only: [then: 3]
  import Kunerauqs.ROP

  @type post_json_param :: %{
          header: list(String.t()),
          data: map()
        }

  def response_ok(data \\ "") do
    KunERAUQS.D0_f.json_encode(%{code: 0, data: data})
  end

  def response_error(code, msg \\ "") when is_integer(code) do
    KunERAUQS.D0_f.json_encode(%{code: code, data: nil, msg: msg})
  end

  @spec json_decode(any) :: any
  def json_decode(str) do
    {:ok, str} >>>
      try_catch((fn d -> {:ok, KunERAUQS.D0_f.json_decode(d)} end).())
  end

  def decode_response(response_data) do
    json_decode(response_data) >>>
      (fn data ->
         case Map.get(data, "code") do
           0 -> {:ok, Map.get(data, "data")}
           error_code -> {:error, {error_code, Map.get(data, "msg")}}
         end
       end).()
  end

  def unknow_error_response_data() do
    response_error(@code_unknown_error)
  end

  # 从远程uri 获取信息
  # ("www.google.com", :get, %{a: 10})
  def get_http(uri) do
    get_http(uri, :get, nil)
  end

  def get_http(uri, method) do
    get_http(uri, method, nil)
  end

  @spec get_http(binary, :get | :post | :post_json, any | post_json_param) :: any
  def get_http(uri, method, params) do
    uri =
      if uri =~ "://" do
        uri
      else
        "http://#{uri}"
      end

    do_get_http(uri, method, params)
  end

  defp do_get_http(uri, :get, nil) do
    # "curl -s --connect-timeout 5 -m 5 \"#{uri}\"" |> IO.inspect()
    # |> IO.inspect()

    case HTTPoison.get(uri, [], []) do
      {:ok, response} ->
        {:ok, response.body}

      _ ->
        {:error, unknow_error_response_data()}
    end
  end

  defp do_get_http(uri, :get, params) do
    # "curl -s --connect-timeout 5 -m 5 \"#{uri}?#{URI.encode_query(params)}\"" |> IO.inspect()
    # |> IO.inspect()

    header = []

    headers =
      if Map.has_key?(params, :header) do
        :lists.append(params.header, header)
      else
        header
      end

    # options = [ssl: [{:versions, [:'tlsv1.2']}], recv_timeout: 500]
    options = []

    case HTTPoison.get("#{uri}?#{URI.encode_query(params)}", headers, options) do
      {:ok, response} ->
        {:ok, response.body}

      _ ->
        {:error, unknow_error_response_data()}
    end
  end

  defp do_get_http(uri, :post, params) do
    # "curl -s --connect-timeout 5 -m 5 -d \"@#{post_tmp_file}\" \"#{uri}\"" |> IO.inspect()
    # |> IO.inspect()

    # "curl -s -d \"#{URI.encode_query(params)}\" \"#{uri}\""
    # |> KunERAUQS.D0_f.runOsCommand()

    header = ["Content-Type": "application/x-www-form-urlencoded"]

    headers =
      if Map.has_key?(params, :header) do
        :lists.append(params.header, header)
      else
        header
      end

    # options = [ssl: [{:versions, [:'tlsv1.2']}], recv_timeout: 500]
    options = []

    body =
      case is_binary(params.body) do
        true -> params.body
        _ -> URI.encode_query(params.body)
      end

    # {body, headers} |> IO.inspect()

    case HTTPoison.post(uri, body, headers, options) do
      {:ok, response} ->
        {:ok, response.body}

      _ ->
        {:error, unknow_error_response_data()}
    end
  end

  defp do_get_http(uri, :post_json, params) do
    header = ["Content-Type": "application/json"]

    headers =
      if Map.has_key?(params, :header) do
        :lists.append(params.header, header)
      else
        header
      end

    # options = [ssl: [{:versions, [:'tlsv1.2']}], recv_timeout: 500]
    options = [timeout: @http_time_out, recv_timeout: @http_time_out]

    body =
      case is_binary(params.body) do
        true -> params.body
        _ -> KunERAUQS.D0_f.json_encode(params.body)
      end

    {uri, body, headers} |> IO.inspect()

    case HTTPoison.post(uri, body, headers, options) do
      {:ok, response} ->
        # response |> IO.inspect()
        {:ok, response.body}

      error ->
        error |> IO.inspect()
        {:error, unknow_error_response_data()}
    end
  end
end
