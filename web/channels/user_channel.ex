defmodule IronfireServer.UserChannel do
	use Phoenix.Channel 

	import Ecto.Query
	alias IronfireServer.Repo
	alias IronfireServer.Settings
	alias IronfireServer.Todo

	def join("user:" <> requested_id, %{"token" => token }, socket) do
    case Phoenix.Token.verify(socket, "user", token) do
      {:ok, user} ->
        if requested_id == user[:id] do
        	send(self, :after_join)
        	{:ok, assign(socket, :user, user)}
        else 
        	{:error, %{reason: "unauthorized"}}
        end
      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end

	end

	def handle_info(:after_join, socket) do
		# get all saved tasks for this user, and shoot them down the pipe
		# also fetch the settings for this user, and push them down
		case Repo.get_by(Settings, user_id: socket.assigns.user[:id]) do 
			%Settings{} = settings -> 
				push socket, "set_settings", (settingsJSON settings)
			_ -> {}
		end
		todos = Repo.all(from t in Todo, where: t.user_id == ^socket.assigns.user[:id])
		case todos do
			nil -> {:noreply, socket}
			_ -> Enum.map todos, fn todo -> push socket, "new_todo", (todoJSON todo) end		  
		end
		{:noreply, socket}
	end

	def handle_in("new_todo", params, socket) do
		# We should add it to our Repo, acknowledge that it was created
		# (the acknowledgement allows the client to set the phxId)
		# and finally broadcast the new task to all connected clients
		changeset = Todo.changeset(%Todo{}, 
			%{user_id: socket.assigns.user[:id],
				text: params["text"],
				status: params["status"],
				times_renewed: params["timesRenewed"],
				last_touched: params["lastWorked"],
				elm_last_modified: params["lastModified"]
			}
		)
		if changeset.valid? do
			newTodo = Repo.insert!(changeset)
			push socket, "ack_todo", %{phxId: newTodo.id, elmId: params["elmId"]}
			broadcast socket, "new_todo", (todoJSON newTodo)
		end
		{:noreply, socket}
	end

	def handle_in("update_todo", params, socket) do
		# Find it in the Repo, update it, and tell everyone
		todo = Repo.get!(Todo, params["phxId"])
		changeset = Todo.changeset(todo, 
			%{user_id: socket.assigns.user[:id],
				text: params["text"],
				status: params["status"],
				times_renewed: params["timesRenewed"],
				last_touched: params["lastWorked"],
				elm_last_modified: params["lastModified"]
			}
		)
		if changeset.valid? && (params["lastModified"] >= todo.elm_last_modified || todo.elm_last_modified == nil) do
			newTodo = Repo.update!(changeset)
			broadcast! socket, "new_todo", (todoJSON newTodo)
		else
			push socket, "new_todo", (todoJSON todo)
		end
		{:noreply, socket}
	end

	def handle_in("set_settings", params, socket) do
		# find previously saved settings for this user, if there are any
		# then write in the updated settings (insert if necessary)
		# and finally broadcast the new settings to all users
		settings = case Repo.get_by(Settings, user_id: socket.assigns.user[:id]) do
			%Settings{} = x -> x
			nil -> %Settings{}
		end
		changeset = Settings.changeset(
			settings, 
			%{user_id: socket.assigns.user[:id],
				freeze_threshold: params["freezeThreshold"],
				cold_check_interval: params["coldCheckInterval"],
				cold_check_interval_unit: params["coldCheckIntervalUnit"],
				cold_length: params["coldLength"],
				cold_length_unit: params["coldLengthUnit"]}
			)
		if changeset.valid? do
			newSettings = Repo.insert_or_update!(changeset)
			broadcast! socket, "set_settings", (settingsJSON newSettings)
		end
		{:noreply, socket}
	end

	defp settingsJSON(%Settings{} = settings) do
		%{freezeThreshold: settings.freeze_threshold,
			coldCheckInterval: settings.cold_check_interval,
			coldCheckIntervalUnit: settings.cold_check_interval_unit,
			coldLength: settings.cold_length,
			coldLengthUnit: settings.cold_length_unit
		}
	end

	defp todoJSON(%Todo{} = todo) do
		%{phxId: todo.id, 
			text: todo.text,
			status: todo.status,
			timesRenewed: todo.times_renewed,
			lastWorked: todo.last_touched,
			lastModified: todo.elm_last_modified,
			saveStatus: "saved"
		}
	end

end