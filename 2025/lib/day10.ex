defmodule AOC2025.Day10 do
  defmodule Machine do
    defstruct goal: [], indicator: [], buttons: [], joltage: []

    def is_valid_indicator(machine) do
      machine.goal == machine.indicator
    end

    def press_button(machine, button, transform) do
      indicator =
        button
        |> Enum.reduce(machine.indicator, fn i, acc ->
          List.replace_at(acc, i, transform.(Enum.at(acc, i)))
        end)

      %{machine | indicator: indicator}
    end
  end

  def solve(path) do
    machines =
      path
      |> File.stream!()
      |> Enum.map(&String.trim/1)
      |> Enum.map(&parse/1)

    part1(machines)
  end

  def part1(machines) do
    machines
    |> Enum.map(&press_vigorously/1)
    |> Enum.sum()
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def press_vigorously(machine) do
    press_vigorously([machine], 1)
  end

  def press_vigorously(machines, pressed) do
    machines =
      machines
      |> Enum.flat_map(fn machine ->
        machine.buttons
        |> Enum.map(fn button -> Machine.press_button(machine, button, &(1 - &1)) end)
      end)

    case Enum.any?(machines, &Machine.is_valid_indicator/1) do
      true -> pressed
      false -> press_vigorously(machines, pressed + 1)
    end
  end

  def parse(line) do
    parts = String.split(line)
    len = length(parts)

    parts
    |> Enum.with_index()
    |> Enum.reduce(%Machine{}, fn {p, i}, acc ->
      cond do
        i == 0 ->
          goal =
            p
            |> String.trim("[")
            |> String.trim("]")
            |> String.graphemes()
            |> Enum.reduce([], fn c, acc ->
              case c do
                "." -> acc ++ [0]
                "#" -> acc ++ [1]
              end
            end)

          indicator = Enum.map(0..(String.length(p) - 3), fn _ -> 0 end)
          %{acc | indicator: indicator, goal: goal}

        i == len - 1 ->
          joltage =
            p
            |> String.trim("{")
            |> String.trim("}")
            |> String.split(",")
            |> Enum.map(&String.to_integer/1)
            |> Enum.to_list()

          %{acc | joltage: joltage}

        true ->
          button =
            p
            |> String.trim("(")
            |> String.trim(")")
            |> String.split(",")
            |> Enum.map(&String.to_integer/1)
            |> Enum.to_list()

          %{acc | buttons: acc.buttons ++ [button]}
      end
    end)
  end
end
