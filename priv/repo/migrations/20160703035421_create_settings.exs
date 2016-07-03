defmodule IronfireServer.Repo.Migrations.CreateSettings do
  use Ecto.Migration

  def change do
    create table(:settings) do
      add :user_id, :string
      add :freeze_threshold, :integer
      add :cold_check_interval, :integer
      add :cold_check_interval_unit, :string
      add :cold_length, :integer
      add :cold_length_unit, :string

      timestamps
    end

  end
end
