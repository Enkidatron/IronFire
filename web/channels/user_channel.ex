defmodule IronfireServer.UserChannel do
	use Phoenix.Channel 

	def join("user:" <> requested_id, %{"token" => token }, socket) do
    case Phoenix.Token.verify(socket, "user", token) do
      {:ok, user} ->
        if requested_id == user[:id] do
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
		{:noreply, socket}
	end

	def handle_in("set_todo", params, socket) do
		# write the incoming task to the Repo, push an acknowledgement, 
		# and broadcast result
		# while writing, check the incoming phxId to see if we already have it
		#   - pattern match on phxId = -1
		IO.puts (inspect params) 
		IO.puts (inspect socket.assigns.user[:id])
		{:noreply, socket}
	end

	def handle_in("set_settings", params, socket) do
		# write the new setting to the settings database, 
		# create a new record for the user's settings if necessary
		# broadcast the new settings
		IO.puts (inspect params)
		{:noreply, socket}
	end

end