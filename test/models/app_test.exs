defmodule IronfireServer.AppTest do
  use IronfireServer.ModelCase

  alias IronfireServer.App

  @valid_attrs %{frozen: true, last_updated: "120.5", user_id: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = App.changeset(%App{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = App.changeset(%App{}, @invalid_attrs)
    refute changeset.valid?
  end
end
