defmodule NostrBackend.TLV do
  @moduledoc false

  defstruct tag: nil, value: nil, indefinite_length: false
end
