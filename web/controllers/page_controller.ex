defmodule IronfireServer.PageController do
  use IronfireServer.Web, :controller

  def index(conn, _params) do
  	conn
  	|> ensure_login
  	|> render("index.html")
  end

  def login(conn, _params) do
  	conn
  	|> assign(:current_user, get_session(conn, :current_user))
  	|> render("login.html")
  end

  def convert(conn, _params) do
    conn 
    |> ensure_login
    |> render("convert.html")
  end

  defp ensure_login(conn) do
		if get_session(conn, :current_user) do
		  conn 
		  |> assign(:current_user, get_session(conn, :current_user))
		  |> assign(:phx_token, Phoenix.Token.sign(conn, "user", get_session(conn, :current_user)))
		else 
			conn
			|> put_flash(:error, "Please log in")
			|> redirect(to: "/login")
		end
  end

end
