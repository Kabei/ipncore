defmodule IppanError do
  defexception message: "Error not defined"
end

defmodule IppanStartUpError do
  defexception message: "Error not defined"
end

defmodule IppanLowError do
  defexception message: "Error not defined"
end

defmodule IppanHighError do
  defexception message: "Error not defined"
end

defmodule IppanCriticalError do
  defexception message: "Error not defined"
end

defmodule IppanRedirectError do
  defexception [:message, :id]

  @impl Exception
  def exception(validator_id) do
    message = "#{validator_id}"

    %__MODULE__{message: message, id: validator_id}
  end
end
