defmodule IronfireServer.Repo.Migrations.AddNotes do
  use Ecto.Migration

  def change do
  	alter table(:todos) do
  		add :notes, :string
  	end
  end
end
