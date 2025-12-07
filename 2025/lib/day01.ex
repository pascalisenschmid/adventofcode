defmodule AOC2025.Day01 do
  def solve(path) do
    path |> File.stream!() |> solve(&part1/1, "Part 1")
    path |> File.stream!() |> solve(&part2/1, "Part 2")
  end

  def solve(file, solver, output) do
    file
    |> Enum.map(&parse_line/1)
    |> solver.()
    |> elem(0)
    |> (fn value -> IO.puts("#{output}: #{value}") end).()
  end

  def part1(input) do
    input
    |> Enum.reduce({0, 50}, fn {dir, amount}, {count, curr} ->
      curr =
        case dir do
          "L" -> curr - amount
          "R" -> curr + amount
        end
        |> Integer.mod(100)

      case curr do
        0 -> {count + 1, curr}
        _ -> {count, curr}
      end
    end)
  end

  def part2(input) do
    input
    |> Enum.reduce({0, 50}, fn {dir, amount}, {count, curr} ->
      new_curr =
        case dir do
          "L" -> curr - amount
          "R" -> curr + amount
        end
        |> Integer.mod(100)

      full_turns = Integer.floor_div(amount, 100)

      case {dir, curr, new_curr} do
        {_, _, _} when curr == new_curr -> {count + full_turns, new_curr}
        {"L", 0, _} when curr < new_curr -> {count + full_turns, new_curr}
        {"L", _, _} when curr < new_curr -> {count + full_turns + 1, new_curr}
        {"R", _, _} when curr > new_curr -> {count + full_turns + 1, new_curr}
        {_, _, 0} -> {count + full_turns + 1, new_curr}
        _ -> {count + full_turns, new_curr}
      end
    end)
  end

  def parse_line(line) do
    line
    |> String.trim()
    |> (fn line ->
          {dir, rest} = String.split_at(line, 1)
          {dir, String.to_integer(rest)}
        end).()
  end
end
