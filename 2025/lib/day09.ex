defmodule AOC2025.Day09 do
  def solve(path) do
    coords =
      path
      |> File.stream!()
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.split(&1, ","))
      |> Enum.map(fn [first, last] -> {String.to_integer(first), String.to_integer(last)} end)

    part1(coords)
    part2(coords)
  end

  def part1(coords) do
    coords
    |> combinations([])
    |> sizes()
    |> Enum.reduce(0, fn {size, _, _}, acc -> max(size, acc) end)
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def part2(coords) do
    edges = edges(coords)
    vertical_edges = vertical_edges(edges)
    horizontal_edges = horizontal_edges(edges)

    on_edge = fn x, y ->
      Enum.any?(horizontal_edges, fn {ey, ex1, ex2} -> ey == y and x >= ex1 and x <= ex2 end) or
        Enum.any?(vertical_edges, fn {ex, ey1, ey2} -> ex == x and y >= ey1 and y <= ey2 end)
    end

    inside_polygon = fn x, y ->
      on_edge.(x, y) or
        Enum.count(vertical_edges, fn {ex, ey1, ey2} ->
          top = ey1 > y
          bottom = ey2 > y
          top != bottom and x < ex
        end)
        |> rem(2) == 1
    end

    vertical_edge_crosses_y = fn min_x, max_x, y ->
      Enum.any?(vertical_edges, fn {ex, ey1, ey2} ->
        ex > min_x and ex < max_x and y > ey1 and y < ey2
      end)
    end

    horizontal_edge_crosses_x = fn min_y, max_y, x ->
      Enum.any?(horizontal_edges, fn {ey, ex1, ex2} ->
        ey > min_y and ey < max_y and x > ex1 and x < ex2
      end)
    end

    rectangle_in_polygon = fn {x1, y1}, {x2, y2} ->
      min_x = min(x1, x2)
      max_x = max(x1, x2)
      min_y = min(y1, y2)
      max_y = max(y1, y2)

      inside_polygon.(min_x, min_y) and
        inside_polygon.(min_x, max_y) and
        inside_polygon.(max_x, min_y) and
        inside_polygon.(max_x, max_y) and
        not vertical_edge_crosses_y.(min_x, max_x, min_y) and
        not vertical_edge_crosses_y.(min_x, max_x, max_y) and
        not horizontal_edge_crosses_x.(min_y, max_y, min_x) and
        not horizontal_edge_crosses_x.(min_y, max_y, max_x)
    end

    coords
    |> combinations([])
    |> sizes()
    |> Enum.sort_by(fn {s, _, _} -> s end, :desc)
    |> Enum.reduce(0, fn {size, p1, p2}, acc ->
      case rectangle_in_polygon.(p1, p2) do
        true -> max(size, acc)
        false -> acc
      end
    end)
    |> (fn res -> IO.puts("Part 2: #{res}") end).()
  end

  def combinations(list, acc) do
    case list do
      [] -> acc
      [hd | tl] -> combinations(tl, acc ++ Enum.map(tl, fn c -> {hd, c} end))
    end
  end

  def sizes(combinations) do
    combinations
    |> Enum.map(fn {{x1, y1}, {x2, y2}} ->
      {(abs(x1 - x2) + 1) * abs(y1 - y2 + 1), {x1, y1}, {x2, y2}}
    end)
  end

  def edges(coords) do
    coords |> Enum.zip(tl(coords) ++ [hd(coords)])
  end

  def vertical_edges(edges) do
    edges
    |> Enum.filter(fn {{x1, _}, {x2, _}} -> x1 == x2 end)
    |> Enum.map(fn {{x, y1}, {_, y2}} -> {x, min(y1, y2), max(y1, y2)} end)
  end

  def horizontal_edges(edges) do
    edges
    |> Enum.filter(fn {{_, y1}, {_, y2}} -> y1 == y2 end)
    |> Enum.map(fn {{x1, y}, {x2, _}} -> {y, min(x1, x2), max(x1, x2)} end)
  end
end
