defmodule MiddleController.Manager.CacheManager do
  require Kunerauqs.GenRamFunction

  Kunerauqs.GenRamFunction.gen_def(
    write_concurrency: true,
    read_concurrency: true,
    # 可选, 缺省为 :set
    type: :set
  )

  use GenServer

  # Client
  def start(init_arg, options \\ []) do
    GenServer.start(__MODULE__, init_arg, [{:name, __MODULE__} | options])
  end

  def start_link(init_arg, options \\ []) do
    GenServer.start_link(__MODULE__, init_arg, [{:name, __MODULE__} | options])
  end

  def store(key, value) do
    GenServer.cast(__MODULE__, {:store, key, value})
  end

  def fetch(key, default \\ nil) do
    GenServer.call(__MODULE__, {:fetch, key, default})
  end

  # Server (callbacks)

  @impl true
  def init(_) do
    init_ram()
    {:ok, nil}
  end

  @impl true
  def handle_call({:fetch, key, default}, _from, state) do
    {:reply, read!(key, default), state}
  end

  @impl true
  def handle_cast({:store, key, value}, state) do
    write(key, value)
    {:noreply, state}
  end
end
