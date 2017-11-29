defmodule IronfireServer.Repo.Migrations.AddWarmMethodToTodo do
  use Ecto.Migration

  def change do
  	alter table(:todos) do
  		add :warm_method, :string
  	end
  end
end
