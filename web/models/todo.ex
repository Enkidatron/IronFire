defmodule IronfireServer.Todo do
  use IronfireServer.Web, :model

  schema "todos" do
    field :user_id, :string
    field :text, :string
    field :status, :string
    field :warm_method, :string
    field :times_renewed, :integer
    field :last_touched, :float
    field :elm_last_modified, :float
    field :socket_id, :integer
    field :elm_id, :integer
    field :notes, :string

    timestamps
  end

  @required_fields ~w(user_id text status times_renewed last_touched elm_last_modified)
  @optional_fields ~w(socket_id elm_id notes warm_method)

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
