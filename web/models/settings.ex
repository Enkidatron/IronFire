defmodule IronfireServer.Settings do
  use IronfireServer.Web, :model

  schema "settings" do
    field :user_id, :string
    field :freeze_threshold, :integer
    field :cold_check_interval, :integer
    field :cold_check_interval_unit, :string
    field :cold_length, :integer
    field :cold_length_unit, :string

    timestamps
  end

  @required_fields ~w(user_id freeze_threshold cold_check_interval cold_check_interval_unit cold_length cold_length_unit)
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
