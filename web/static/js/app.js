// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"

// make all of this less terrible. By which I mean, 
// put it in its own page-specific file somehow
import Elm from "./main"
const elmDiv = document.querySelector('#elm-target');
if (elmDiv) {
	var ironfire = Elm.Main.fullscreen({userid: window.userId, token: window.userToken, phxUrl: window.phxUrl});
	ironfire.ports.connectLocal.subscribe(function(userid){
	  setTimeout(function() {
	    var savedSettings = localStorage.getItem("elm-ironfire-" + userid + "-settings");
	    if(savedSettings){
	    	ironfire.ports.rxSettings.send(JSON.parse(savedSettings));
	    }
	    var savedTodos = localStorage.getItem("elm-ironfire-" + userid + "-todos");
	    if (savedTodos) {
	    	var todos = JSON.parse(savedTodos);
	    	ironfire.ports.rxTodos.send(todos);
	    }
	    var savedAppStatus = localStorage.getItem("elm-ironfire-" + userid + "-appstatus");
	    if (savedAppStatus) {
	    	ironfire.ports.rxAppStatus.send(JSON.parse(savedAppStatus));
	    }
	  }, 50);
	});
	ironfire.ports.saveTodosLocal.subscribe(function(params){
	  setTimeout(function() {
	  	params = JSON.parse(params);
	    localStorage.setItem("elm-ironfire-" + params.userid + "-todos", JSON.stringify(params.todos));
	  }, 50);
	});
	ironfire.ports.saveAppStatusLocal.subscribe(function(params){
		setTimeout(function(){
			params = JSON.parse(params);
			localStorage.setItem("elm-ironfire-" + params.userid + "-appstatus", JSON.stringify(params.appstatus));
		}, 50);
	});
	ironfire.ports.saveSettingsLocal.subscribe(function(params){
	  setTimeout(function() {
	  	params = JSON.parse(params);
	    localStorage.setItem("elm-ironfire-" + params.userid + "-settings", JSON.stringify(params.settings));
	  }, 50);
	});
}