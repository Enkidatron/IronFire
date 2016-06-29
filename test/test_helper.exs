ExUnit.start

Mix.Task.run "ecto.create", ~w(-r IronfireServer.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r IronfireServer.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(IronfireServer.Repo)

