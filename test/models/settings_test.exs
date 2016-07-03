defmodule IronfireServer.SettingsTest do
  use IronfireServer.ModelCase

  alias IronfireServer.Settings

  @valid_attrs %{cold_check_interval: 42, cold_check_interval_unit: "some content", cold_length: 42, cold_length_unit: "some content", freeze_threshold: 42, user_id: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Settings.changeset(%Settings{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Settings.changeset(%Settings{}, @invalid_attrs)
    refute changeset.valid?
  end
end
