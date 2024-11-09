defmodule NostrBackend.PostHogBuffer do
  use GenServer

  # 1 second in milliseconds
  @send_interval 1_000

  # Start the GenServer
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_state) do
    # Initialize the state with an empty list of events
    initial_state = %{events: []}

    # Schedule the first batch send
    schedule_send()
    {:ok, initial_state}
  end

  # Public function to add an event to the buffer
  def add_event(event_data) do
    GenServer.cast(__MODULE__, {:add_event, event_data})
  end

  # Handle adding events
  def handle_cast({:add_event, event_data}, state) do
    # Add the event to the buffer
    new_state = Map.update(state, :events, [event_data], fn events -> [event_data | events] end)
    {:noreply, new_state}
  end

  # Handle the scheduled batch send
  def handle_info(:send_events, %{events: []} = state) do
    # No events to send, reschedule and continue
    schedule_send()
    {:noreply, state}
  end

  def handle_info(:send_events, %{events: events} = state) do
    # Only send the batch if there are events
    if length(events) > 0 do
      events
      |> Posthog.batch()
    end

    # Clear the events and reschedule
    schedule_send()
    {:noreply, %{state | events: []}}
  end

  # Schedule the next send after the interval
  defp schedule_send do
    Process.send_after(self(), :send_events, @send_interval)
  end
end
