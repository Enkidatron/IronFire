defmodule IronfireServer.App do
  use IronfireServer.Web, :model

  schema "apps" do
    field :user_id, :string
    field :frozen, :boolean, default: false
    field :last_updated, :float

    timestamps
  end

  @required_fields ~w(user_id frozen last_updated)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
