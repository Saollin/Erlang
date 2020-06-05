defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV() do
    filename = "pollution.csv"
    File.read!(filename) |>
      String.split(~r/\R/)
  end

  def parseOneLine(line) do
    [date, hour, x, y, value] = line |>
      String.split(",")
    date =
      date |>
        String.split("-") |>
        Enum.reverse() |>
        Enum.map(fn v -> Integer.parse(v) end) |>
        Enum.map(fn v -> elem(v, 0) end) |>
        :erlang.list_to_tuple()
    {hours, minutes} = hour |>
      String.split(":") |>
      Enum.map(fn v -> Integer.parse(v) end) |>
      Enum.map(fn v -> elem(v, 0) end) |>
      :erlang.list_to_tuple()
    coord = [x, y] |>
      Enum.map(fn v -> Float.parse(v) end) |>
      Enum.map(fn v -> elem(v, 0) end) |>
      :erlang.list_to_tuple()
    value = elem(Integer.parse(value), 0)
    %{:datetime => {date, {hours, minutes, 0}}, :location => coord, :pollutionLevel => value}
  end

  def identifyStations(data) do
    unique = data |>
      Enum.uniq_by(fn one -> one[:location] end)
    for x <- unique, do: x[:location]
  end

  def loadStation(station_coord) do
    string = "station_#{elem(station_coord, 0)}_#{elem(station_coord, 1)}"
    :pollution_gen_server.addStation(string, station_coord)
  end

  def loadStations(stations) do
    Enum.each(stations, fn x -> loadStation(x) end)
  end

  def loadValues(values) do
    Enum.each(values, fn x -> loadValue(x) end)
  end

  def loadValue(value) do
    name = value[:location]
    date = value[:datetime]
    given_value = value[:pollutionLevel]
    :pollution_gen_server.addValue(name, date, "PM10", given_value)
  end

  def loadData() do
    lines = importLinesFromCSV()
    values = for x <- lines, do: parseOneLine(x)
    stations = identifyStations(values)
    load_station_time = checkTime(fn -> loadStations(stations) end)
    load_values_time = checkTime(fn -> loadValues(values) end)
    {load_station_time, load_values_time}
  end

  def checkTime(func) do
    func |>
      :timer.tc() |>
      elem(0) |>
      Kernel./(1_000_000)
  end
end

