defmodule AOC2025.Day07 do
  alias Common.Matrix

  def solve(path) do
    matrix = path |> Matrix.from()
    start = matrix |> Matrix.find_single("S")
    part1(matrix, start)
    part2(matrix, start)
  end

  def part1(matrix, start) do
    beam(matrix, [start], MapSet.new())
    |> MapSet.size()
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def part2(matrix, start) do
    timelines(matrix, start, %{})
    |> (fn {res, _} -> IO.puts("Part 2: #{res}") end).()
  end

  def beam(matrix, from, split_at) do
    case from do
      [] ->
        split_at

      [hd | tl] ->
        case move_until(matrix, hd, :down, "^") do
          {:ok, c} ->
            new_split_at = split_at |> MapSet.put(c)

            case MapSet.size(split_at) == MapSet.size(new_split_at) do
              true ->
                beam(matrix, tl, new_split_at)

              false ->
                beam(matrix, [Matrix.move(c, :left), Matrix.move(c, :right)] ++ tl, new_split_at)
            end

          {:oob, _} ->
            beam(matrix, tl, split_at)
        end
    end
  end

  def timelines(matrix, from, visited) do
    case move_until(matrix, from, :down, "^") do
      {:ok, c} ->
        {left, visited} = get_or_set(visited, Matrix.move(c, :left), &timelines(matrix, &2, &1))
        {right, visited} = get_or_set(visited, Matrix.move(c, :right), &timelines(matrix, &2, &1))
        {left + right, visited}

      {:oob, _} ->
        {1, visited}
    end
  end

  def get_or_set(map, key, factory) do
    case Map.get(map, key, nil) do
      nil ->
        {val, map} = factory.(map, key)
        map = Map.put(map, key, val)
        {val, map}

      val ->
        {val, map}
    end
  end

  def move_until(matrix, curr, dir, target) do
    case Matrix.get(matrix, curr) do
      {:ok, val} when val == target -> {:ok, curr}
      {:ok, _} -> move_until(matrix, Matrix.move(curr, dir), dir, target)
      {:oob, _} -> {:oob, nil}
    end
  end
end
