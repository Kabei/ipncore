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
  defexception message: "Redirect to correct validator"
end
