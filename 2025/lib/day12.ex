defmodule AOC2025.Day12 do
  def solve(path) do
    {presents, regions} =
      path
      |> File.stream!()
      |> split_on_empty()
      |> parse()

    regions
    |> Enum.count(&can_fit?(&1, presents))
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def can_fit?({{x, y}, amounts}, presents) do
    min_size =
      amounts
      |> Enum.with_index()
      |> Enum.reduce(0, fn {amount, i}, acc ->
        acc + Map.get(presents, i) * amount
      end)

    min_size <= x * y
  end

  def parse(chunks) do
    parse_chunks(chunks, {%{}, []})
  end

  defp parse_chunks([last], {presents, _regions}) do
    regions =
      last
      |> Enum.map(&String.split(&1, ":", trim: true))
      |> Enum.map(fn [area, amounts] ->
        area = area |> String.split("x") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
        amounts = amounts |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)
        {area, amounts}
      end)

    {presents, regions}
  end

  defp parse_chunks([hd | tl], {presents, regions}) do
    [index | present] = hd
    key = index |> String.graphemes() |> List.first() |> String.to_integer()

    area =
      present
      |> Enum.reduce(0, fn line, acc ->
        acc + (line |> String.graphemes() |> Enum.count(&(&1 == "#")))
      end)

    parse_chunks(tl, {Map.put_new(presents, key, area), regions})
  end

  def split_on_empty(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.filter(&(length(&1) > 1))
  end
end
