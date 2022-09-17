defmodule SKA do
  @moduledoc """
  Symmetric Key Algorithm (SKA)
  """
  use AESGCM, max_age: 120
end
