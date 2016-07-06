defmodule IronfireServer.Repo.Migrations.AddIndexes do
  use Ecto.Migration

  def change do

  	create index(:todos, [:user_id])
  	create index(:settings, [:user_id])
  end
end
