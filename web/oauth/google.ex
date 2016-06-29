defmodule Google do 
	use OAuth2.Strategy

	alias OAuth2.Strategy.AuthCode

	def client do
		OAuth2.Client.new([
			strategy: Google,
			client_id: System.get_env("CLIENT_ID"),
			client_secret: System.get_env("CLIENT_SECRET"),
			redirect_uri: System.get_env("REDIRECT_URI"),
			site: "https://accounts.google.com",
			authorize_url: "/o/oauth2/auth",
			token_url: "https://www.googleapis.com/oauth2/v4/token"
		])
	end

	def authorize_url!(params \\ []) do
		OAuth2.Client.authorize_url!(client(), params)
	end

	def get_token!(params \\ [], headers \\ []) do
    OAuth2.Client.get_token!(client(), params)
	end


	# strategy callbacks

	def authorize_url(client, params) do
		AuthCode.authorize_url(client, params)
	end

	def get_token(client, params, headers) do
		client 
		|> put_header("Accept", "application/json")
		|> AuthCode.get_token(params,headers)
	end

end