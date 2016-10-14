defmodule IronfireServer.Repo.Migrations.CreateApp do
  use Ecto.Migration

  def change do
    create table(:apps) do
      add :user_id, :string
      add :frozen, :boolean, default: false
      add :last_updated, :float

      timestamps
    end

    create index(:apps, [:user_id])
  end
end
