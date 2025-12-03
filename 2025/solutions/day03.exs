defmodule Day03 do
  def solve(file, digits, output) do
    file
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(fn chars -> Enum.map(chars, &String.to_integer/1) end)
    |> Enum.map(&find_largest(&1, digits, [], -1, []))
    |> Enum.sum()
    |> (fn res -> IO.puts("#{output}: #{res}") end).()
  end

  def find_largest(list, curr_digit, acc, largest, rem) do
    case curr_digit do
      0 ->
        acc |> Enum.join() |> String.to_integer()

      _ ->
        case length(list) < curr_digit do
          true ->
            find_largest(rem, curr_digit - 1, acc ++ [Integer.to_string(largest)], -1, [])

          false ->
            case list do
              [hd | tl] when hd > largest -> find_largest(tl, curr_digit, acc, hd, tl)
              [hd] when hd > largest -> find_largest([], curr_digit, acc, hd, [])
              [_ | tl] -> find_largest(tl, curr_digit, acc, largest, rem)
            end
        end
    end
  end
end

path = "inputs/day03.txt"
path |> File.stream!() |> Day03.solve(2, "Part 1")
path |> File.stream!() |> Day03.solve(12, "Part 2")
