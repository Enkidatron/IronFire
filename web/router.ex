defmodule IronfireServer.Router do
  use IronfireServer.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", IronfireServer do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/login", PageController, :login
    get "/convert", PageController, :convert
  end

  # Other scopes may use custom stacks.
  # scope "/api", IronfireServer do
  #   pipe_through :api
  # end

  scope "/oauth", IronfireServer do
    pipe_through :browser

    get "/:provider", AuthController, :index
    get "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
  end
end
