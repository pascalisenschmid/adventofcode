defmodule AOC2025.Day08 do
  defmodule Coord do
    defstruct [:x, :y, :z]

    def from_list(list) do
      [x, y, z] = list
      %Coord{x: String.to_integer(x), y: String.to_integer(y), z: String.to_integer(z)}
    end

    def distance(%{x: x1, y: y1, z: z1}, %{x: x2, y: y2, z: z2}) do
      :math.sqrt(:math.pow(x1 - x2, 2) + :math.pow(y1 - y2, 2) + :math.pow(z1 - z2, 2))
    end
  end

  def solve(path) do
    coords =
      File.stream!(path)
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.split(&1, ","))
      |> Enum.map(&Coord.from_list/1)

    dist_pairs =
      coords
      |> calc_all_distances([])
      |> Enum.sort_by(fn {_, _, d} -> d end)

    circuits = coords |> Enum.map(&[&1])

    part1(dist_pairs, circuits)
    part2(dist_pairs, circuits)
  end

  def part1(dist_pairs, circuits) do
    connect(dist_pairs, 1000, circuits)
    |> Enum.map(&length(&1))
    |> Enum.sort(&>=/2)
    |> Enum.take(3)
    |> Enum.reduce(1, &*/2)
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def part2(dist_pairs, circuits) do
    connect_all(dist_pairs, circuits)
    |> (fn res -> IO.puts("Part 2: #{res}") end).()
  end

  def calc_all_distances(list, acc) do
    case list do
      [_] ->
        acc

      [hd | tl] ->
        acc =
          Enum.map(tl, fn c ->
            {hd, c, Coord.distance(hd, c)}
          end) ++ acc

        calc_all_distances(tl, acc)
    end
  end

  def connect_all(list, circuits) do
    [hd | tl] = list
    circuits = connect([hd], 1, circuits)

    if length(circuits) == 1 do
      {%Coord{x: x1, y: _, z: _}, %Coord{x: x2, y: _, z: _}, _} = hd
      x1 * x2
    else
      connect_all(tl, circuits)
    end
  end

  def connect(list, amount, circuits) do
    case {list, amount} do
      {_, 0} ->
        circuits

      {[{c1, c2, _} | tl], _} ->
        containing_c1 = circuits |> Enum.find(&Enum.member?(&1, c1))
        containing_c2 = circuits |> Enum.find(&Enum.member?(&1, c2))

        case containing_c1 == containing_c2 do
          true ->
            connect(tl, amount - 1, circuits)

          false ->
            circuits = circuits |> List.delete(containing_c1) |> List.delete(containing_c2)
            circuits = circuits ++ [Enum.concat(containing_c1, containing_c2)]
            connect(tl, amount - 1, circuits)
        end
    end
  end
end
