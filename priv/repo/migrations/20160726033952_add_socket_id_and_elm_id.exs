defmodule IronfireServer.Repo.Migrations.AddSocketIdAndElmId do
  use Ecto.Migration

  def change do
  	alter table(:todos) do
  		add :socket_id, :bigint
  		add :elm_id, :integer
  	end

  	create index(:todos, [:socket_id, :elm_id])
  end
end
