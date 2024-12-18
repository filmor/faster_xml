defmodule MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :faster_xml,
      version: @version,
      deps: deps(),
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.35.1"}
    ]
  end

end
