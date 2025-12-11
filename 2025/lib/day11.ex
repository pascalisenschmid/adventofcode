defmodule AOC2025.Day11 do
  def solve(path) do
    network =
      path
      |> File.stream!()
      |> Enum.map(&String.trim/1)
      |> Enum.to_list()
      |> parse()

    part1(network)
    part2(network)
  end

  def part1(network) do
    count_paths(network, "you", "out")
    |> (fn res -> IO.puts("Part 1: #{res}") end).()
  end

  def part2(network) do
    (count_paths(network, "svr", "dac") *
       count_paths(network, "dac", "fft") *
       count_paths(network, "fft", "out") +
       count_paths(network, "svr", "fft") *
         count_paths(network, "fft", "dac") *
         count_paths(network, "dac", "out"))
    |> (fn res -> IO.puts("Part 2: #{res}") end).()
  end

  def count_paths(network, start, target) do
    find_path(network, %{}, start, target) |> elem(0)
  end

  def find_path(_network, paths, current, target) when current == target, do: {1, paths}

  def find_path(network, paths, current, target) do
    case paths[current] do
      nil ->
        {count, new_paths} =
          network
          |> Map.get(current, [])
          |> Enum.reduce({0, paths}, fn neighbor, {acc_count, acc_paths} ->
            {n_count, acc_paths} = find_path(network, acc_paths, neighbor, target)
            {acc_count + n_count, acc_paths}
          end)

        {count, Map.put(new_paths, current, count)}

      cached ->
        {cached, paths}
    end
  end

  def parse(lines) do
    lines
    |> Enum.map(&String.split(&1, ":", trim: true))
    |> Enum.reduce(%{}, fn [key, value], acc ->
      value |> String.split(" ", trim: true) |> then(&Map.put(acc, key, &1))
    end)
  end
end
