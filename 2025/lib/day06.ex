defmodule AOC2025.Day06 do
  def solve(path) do
    path |> File.stream!() |> solve(&parse_part1/1, "Part 1")
    path |> File.stream!() |> solve(&parse_part2/1, "Part 2")
  end

  def solve(lines, parser, output) do
    lines
    |> parser.()
    |> Enum.map(fn {nums, op} ->
      case op do
        "*" -> nums |> Enum.reduce(1, &*/2)
        "+" -> nums |> Enum.sum()
      end
    end)
    |> Enum.sum()
    |> (fn res -> IO.puts("#{output}: #{res}") end).()
  end

  def parse_part1(lines) do
    lines = lines |> Enum.map(&String.split/1)
    parts = length(lines)
    problems = length(Enum.at(lines, 1))

    Enum.map(0..(problems - 1), fn idx ->
      operator = Enum.at(Enum.at(lines, -1), idx)

      nums =
        Enum.map(0..(parts - 2), fn i ->
          Enum.at(Enum.at(lines, i), idx) |> String.to_integer()
        end)

      {nums, operator}
    end)
  end

  def parse_part2(lines) do
    to_int = fn chars -> chars |> Enum.join() |> String.trim() |> String.to_integer() end

    lines
    |> Enum.map(&String.trim_trailing/1)
    |> Enum.map(&String.graphemes/1)
    |> pad_lines()
    |> Enum.map(&Enum.reverse/1)
    |> rotate()
    |> Enum.reduce({[], {[], ""}}, fn col, {acc, {nums, op}} ->
      case {Enum.all?(col, &(&1 == " ")), Enum.at(col, -1)} do
        {true, _} -> {acc ++ [{nums, op}], {[], ""}}
        {_, " "} -> {acc, {nums ++ [col |> to_int.()], op}}
        {_, op} -> {acc, {nums ++ [col |> Enum.drop(-1) |> to_int.()], op}}
      end
    end)
    |> (fn {acc, last} -> acc ++ [last] end).()
  end

  def pad_lines(lines) do
    longest = lines |> Enum.map(&length/1) |> Enum.max()

    lines
    |> Enum.map(fn chars ->
      case longest - length(chars) do
        0 -> chars
        pad -> chars ++ Enum.map(1..pad, fn _ -> " " end)
      end
    end)
  end

  def rotate(lines) do
    line_count = length(lines)
    line_length = length(Enum.at(lines, 1))

    Enum.map(0..(line_length - 1), fn col_idx ->
      Enum.map(0..(line_count - 1), fn i ->
        Enum.at(Enum.at(lines, i), col_idx)
      end)
    end)
  end
end
