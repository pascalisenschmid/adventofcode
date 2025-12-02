defmodule Day02 do
  def solve(file, solver, output) do
    file
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.split(&1, "-"))
    |> Enum.map(fn lst ->
      {List.first(lst) |> String.to_integer(), List.last(lst) |> String.to_integer()}
    end)
    |> Enum.map(fn {min, max} -> solver.(min, max, 0) end)
    |> Enum.sum()
    |> (fn res -> IO.puts("#{output}: #{res}") end).()
  end

  def part1(curr, max, acc) when curr <= max do
    case String.split_at("#{curr}", div(String.length("#{curr}"), 2)) do
      {a, b} when a == b -> part1(curr + 1, max, acc + curr)
      _ -> part1(curr + 1, max, acc)
    end
  end

  def part1(_, _, acc) do
    acc
  end

  def part2(curr, max, acc) when curr <= max do
    case repeating?(curr) do
      true -> part2(curr + 1, max, acc + curr)
      false -> part2(curr + 1, max, acc)
    end
  end

  def part2(_, _, acc) do
    acc
  end

  def repeating?(number) do
    digits = Integer.to_string(number)
    len = String.length(digits)

    len > 1 and
      Enum.any?(1..div(len, 2), fn size ->
        rem(len, size) == 0 and
          String.duplicate(String.slice(digits, 0, size), div(len, size)) == digits
      end)
  end
end

path = "inputs/day02.txt"
path |> File.read!() |> Day02.solve(&Day02.part1/3, "Part 1")
path |> File.read!() |> Day02.solve(&Day02.part2/3, "Part 2")
