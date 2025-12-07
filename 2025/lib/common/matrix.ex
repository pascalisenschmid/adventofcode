defmodule Common.Matrix do
  defmodule Coord do
    defstruct [:x, :y]
  end

  defmodule Dir do
    @type t :: :up | :down | :left | :right
  end

  def from(path) do
    path
    |> File.stream!()
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.graphemes/1)
    |> Enum.to_list()
  end

  def find_single(matrix, char) do
    max_y = length(matrix) - 1
    max_x = length(Enum.at(matrix, 0)) - 1

    Enum.reduce(0..max_y, %Coord{}, fn y, coord ->
      Enum.reduce(0..max_x, coord, fn x, coord ->
        case Enum.at(Enum.at(matrix, y), x) == char do
          true -> %Coord{x: x, y: y}
          false -> coord
        end
      end)
    end)
  end

  def move(%Coord{x: x, y: y}, dir) do
    case dir do
      :up -> %Coord{x: x, y: y - 1}
      :down -> %Coord{x: x, y: y + 1}
      :right -> %Coord{x: x + 1, y: y}
      :left -> %Coord{x: x - 1, y: y}
    end
  end

  def get(matrix, %Coord{x: x, y: y}) do
    max_y = length(matrix) - 1
    max_x = length(Enum.at(matrix, 0)) - 1

    case {x, y} do
      {x, y} when x < 0 or x > max_x or y < 0 or y > max_y -> {:oob, nil}
      {x, y} -> {:ok, Enum.at(Enum.at(matrix, y), x)}
    end
  end
end
