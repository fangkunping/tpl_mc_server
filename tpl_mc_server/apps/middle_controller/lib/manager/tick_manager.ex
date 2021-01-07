defmodule MiddleController.Manager.TickManager do
  use GenServer

  alias MiddleController.App.SampleTimerEvent
  alias MiddleController.Manager.TickManager, as: State
  defstruct [:event_timer_list]

  # 3秒一次ticket
  @callback_time 3000

  # Client
  def start(init_arg, options \\ []) do
    GenServer.start(__MODULE__, init_arg, options)
  end

  def start_link(init_arg, options \\ []) do
    GenServer.start_link(__MODULE__, init_arg, options)
  end

  def times_up(pid) do
    pid |> GenServer.call({:times_up})
  end

  # Server (callbacks)

  @impl true
  def init(_) do
    setup_timer(@callback_time)

    due_event = SampleTimerEvent.new()

    {:ok,
     %State{
       event_timer_list: [
         fn -> SampleTimerEvent.tick(due_event) end
       ]
     }}
  end

  # 定时时间到
  @impl true
  def handle_call({:times_up}, _form, %State{event_timer_list: event_timer_list} = state) do
    "times up" |> IO.inspect()
    event_timer_list
    |> Enum.each(fn event_timer_fn ->
      event_timer_fn.()
    end)

    setup_timer(@callback_time)
    {:reply, :ok, state}
  end

  # Local
  defp setup_timer(callback_time) do
    :timer.apply_after(callback_time, __MODULE__, :times_up, [self()])
  end
end
