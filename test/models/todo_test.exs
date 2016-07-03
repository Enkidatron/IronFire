defmodule IronfireServer.TodoTest do
  use IronfireServer.ModelCase

  alias IronfireServer.Todo

  @valid_attrs %{last_touched: "120.5", text: "some content", status: "more content", times_renewed: 42, user_id: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Todo.changeset(%Todo{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Todo.changeset(%Todo{}, @invalid_attrs)
    refute changeset.valid?
  end
end
