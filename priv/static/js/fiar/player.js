Players = {
  connected : [],
  current : undefined,
  init : function(users) {
    users.forEach(Players.add);
  },
  add : function(player) {
    var playerExists = Players._findById(player.user.id);
    if(player.user.id != Players.current.user.id && !playerExists)
      Players.connected.push(player);
  },
  updateStatus : function(playerId, match) {
    var player = Players._findById(playerId);
    if(player)
        player.current_matches = match ? [match] : [];
  },
  loadCurrent : function() {
    var url = "/me";
    var method = "GET";
    var data = "";
    var success = function(data) {
      Players.current = data;
      Players.current.busy = data.current_matches.length > 0;
    };
    Utils.sendRequest(url, method, data, success);
  },
  updateCurrent :function(match, event) {
    var playerId = Players.current.user.id;
    if (playerId == match.player1 || playerId == match.player2) {
      if(event == "start") 
        Players.current.current_matches = [match];
      else if(event == "end")
        Players.current.current_matches = [];
    }
  },
  isBusy : function(player) {
   return player.current_matches.length > 0;
  },
  updateView :function() {
    $("ul#players_online").empty();
    Players.connected.forEach(Players._updateViewPlayer);
  },
  _updateViewPlayer : function(player) {
    var li = $("<li></li>")
               .addClass('player')
               .attr('id', 'player_' + player.user.id)
               .text(player.user.username);
    var busy = Players.isBusy(player);
    if(busy)
      li.addClass("busy");
    else
      li.addClass("available");

    $("ul#players_online").append(li);
  },
  _findById : function(id) {
    var found = undefined;
    Players.connected.forEach(function(player) {
      if(player.user.id == id) found = player;
    });
    return found;
  }
};