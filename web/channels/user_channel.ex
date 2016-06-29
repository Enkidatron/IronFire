defmodule IronfireServer.UserChannel do
	use Phoenix.Channel 

	def join("user:" <> requested_id, _message, socket) do
		if requested_id == socket.assigns.user[:id] do
		  send(self, :after_join)
		  {:ok, socket}
		else
			{:error, %{reason: "unauthorized"}}
		end
	end

	def handle_info(:after_join, socket) do
		# get all saved tasks for this user, and shoot them down the pipe
		{:noreply, socket}
	end

	def handle_in("new_task", %{"body" => body}, socket) do
		# write the incoming task to the Repo and broadcast result
		# or push back an error message
		{:noreply, socket}
	end
end