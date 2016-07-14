defmodule IronfireServer.Repo.Migrations.AddElmLastModified do
  use Ecto.Migration

  def change do
  	alter table(:todos) do
  		add :elm_last_modified, :float
  	end
  end
end
