defmodule IronfireServer.Repo.Migrations.AddStatusToTodos do
  use Ecto.Migration

  def change do
  	alter table(:todos) do
  		add :status, :string
  	end
  end
end
