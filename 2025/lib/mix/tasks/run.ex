defmodule Mix.Tasks.Solve do
  use Mix.Task

  def run([day]) do
    module = Module.concat(AOC2025, "Day" <> String.pad_leading(day, 2, "0"))
    input = "inputs/day#{String.pad_leading(day, 2, "0")}.txt"

    apply(module, :solve, [input])
  end
end
