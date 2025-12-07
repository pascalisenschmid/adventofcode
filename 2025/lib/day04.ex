defmodule AOC2025.Day04 do
  def solve(path) do
    path
    |> File.stream!()
    |> solve(&remove/1)
    |> (fn {removed, _} -> IO.puts("Part 1: #{removed}") end).()

    path
    |> File.stream!()
    |> solve(&remove_all/1)
    |> (fn removed -> IO.puts("Part 2: #{removed}") end).()
  end

  def solve(file, solver) do
    file
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.graphemes/1)
    |> Enum.to_list()
    |> solver.()
  end

  def remove_all(matrix) do
    case remove(matrix) do
      {0, _} -> 0
      {removed, new_matrix} -> removed + remove_all(new_matrix)
    end
  end

  def remove(matrix) do
    max_y = length(matrix) - 1
    max_x = length(Enum.at(matrix, 0)) - 1

    Enum.reduce(0..max_y, {0, []}, fn y, {removed, out} ->
      {removed, new_row} =
        Enum.reduce(0..max_x, {removed, []}, fn x, {acc, x_out} ->
          case Enum.at(Enum.at(matrix, y), x) do
            "@" ->
              case count_adjacent(matrix, x, y) do
                r when r < 4 -> {acc + 1, x_out ++ ["."]}
                _ -> {acc, x_out ++ ["@"]}
              end

            _ ->
              {acc, x_out ++ ["."]}
          end
        end)

      {removed, out ++ [new_row]}
    end)
  end

  def count_adjacent(matrix, x, y) do
    Enum.reduce(get_dirs(x, y), 0, fn {x_, y_}, count ->
      case get_safe(matrix, x_, y_) do
        "@" -> count + 1
        _ -> count
      end
    end)
  end

  def get_safe(matrix, x, y) do
    case {x, y} do
      {x, y} when x < 0 or y < 0 -> "."
      _ -> Enum.at(Enum.at(matrix, y, []), x, ".")
    end
  end

  def get_dirs(x, y) do
    [
      {x - 1, y - 1},
      {x - 1, y},
      {x - 1, y + 1},
      {x, y - 1},
      {x, y + 1},
      {x + 1, y - 1},
      {x + 1, y},
      {x + 1, y + 1}
    ]
  end
end
