defmodule MiddleController.App.SampleTimerEvent do
  @behaviour MiddleController.Interface.ITimerEvent
  alias MiddleController.App.SampleTimerEvent, as: State
  defstruct [:next_update_ms, :checking?]

  import Kunerauqs.CommonTools

  # 每10秒 一次
  @check_interval_ms 10000

  def new() do
    state = %State{next_update_ms: 0, checking?: false}
    {:ok, pid} = Agent.start(fn -> state end)
    pid
  end

  def tick(pid) do
    now = Kunerauqs.CommonTools.timestamp_ms()

    Agent.cast(pid, fn
      # 还在访问php，不进行操作
      %State{checking?: true} = state ->
        state

      # 还没有到达下次检查时间点，不进行操作
      %State{next_update_ms: next_update_ms, checking?: false} = state
      when now < next_update_ms ->
        state

      %State{} = state ->
        say_hello(pid)
        %State{state | checking?: true}
    end)
  end

  # 调用php进行过期检测完成后，调用该函数，对状态进行重置
  defp check_done(pid) do
    Agent.cast(pid, fn %State{} = state ->
      now = Kunerauqs.CommonTools.timestamp_ms()
      next_update_ms = now + @check_interval_ms
      %State{state | checking?: false, next_update_ms: next_update_ms}
    end)
  end

  # 访问php进行过期检测
  defp say_hello(pid) do
    spawn(fn ->
      "Saying Hello" |> IO.inspect()

      check_done(pid)
    end)
  end
end
