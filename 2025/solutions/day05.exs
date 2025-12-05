defmodule Day05 do
  def part1({ranges, nums}) do
    nums
    |> Enum.map(fn ingredient ->
      Enum.any?(ranges, fn {start, ending} ->
        ingredient in start..ending
      end)
    end)
    |> Enum.count(& &1)
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def part2({ranges, _}) do
    ranges
    |> Enum.reduce([], fn range, acc ->
      acc ++ p2([range], acc)
    end)
    |> Enum.map(fn {start, ending} ->
      ending - start + 1
    end)
    |> Enum.sum()
    |> (fn res -> IO.puts("Part 2: #{res}") end).()
  end

  def p2(new_ranges, ranges) do
    case ranges do
      [] -> new_ranges
      [hd | tl] -> new_ranges |> Enum.map(&Day05.fix_overlaps(hd, &1)) |> Enum.concat() |> p2(tl)
    end
  end

  def fix_overlaps(range, new_range) do
    case {range, new_range} do
      {{s, e}, {sn, en}} when sn >= s and en <= e -> []
      {{s, e}, {sn, en}} when sn > e or en < s -> [new_range]
      {{s, e}, {sn, en}} when sn < s and en >= s and en <= e -> [{sn, s - 1}]
      {{s, e}, {sn, en}} when en > e and sn <= e and sn >= s -> [{e + 1, en}]
      {{s, e}, {sn, en}} -> [{sn, s - 1}, {e + 1, en}]
    end
  end

  def parse(lines) do
    parse_part1(lines, {[], []})
  end

  def parse_part1(lns, {acc, p2}) do
    case lns do
      ["" | tl] ->
        parse_part2(tl, {acc, p2})

      [hd | tl] ->
        parts = hd |> String.split("-")
        start = parts |> List.first() |> String.to_integer()
        ending = parts |> List.last() |> String.to_integer()
        parse_part1(tl, {acc ++ [{start, ending}], p2})
    end
  end

  def parse_part2(lns, {p1, acc}) do
    case lns do
      [] -> {p1, acc}
      [hd | tl] -> parse_part2(tl, {p1, acc ++ [String.to_integer(hd)]})
    end
  end
end

path = "inputs/day05.txt"

parsed = path |> File.stream!() |> Enum.map(&String.trim/1) |> Day05.parse()
parsed |> Day05.part1()
parsed |> Day05.part2()
