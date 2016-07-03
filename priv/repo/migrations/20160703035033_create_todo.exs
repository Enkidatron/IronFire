defmodule IronfireServer.Repo.Migrations.CreateTodo do
  use Ecto.Migration

  def change do
    create table(:todos) do
      add :user_id, :string
      add :text, :string
      add :times_renewed, :integer
      add :last_touched, :float

      timestamps
    end

  end
end
