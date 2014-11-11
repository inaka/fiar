Match = {
  start : function(event){
    var username = event.target.innerText;
    var url = "/matches";
    var method = "POST";
    var data = JSON.stringify({'player2': username});
    Utils.sendRequest(url, method, data, Match.connect);
  },
  end : function(){
    Players.current.current_matches.forEach(function (match) {
      var url = "/matches/"+match.id;
      var method = "DELETE";
      var data = "";
      Utils.sendRequest(url, method, data);
    });
  },
  connect : function(data) {
    var es = new EventSource("/matches/" + data.id + "/events");
    Board.enable();
    console.log("ready state");
    console.log("start listen ME");
    es.addEventListener('turn', function(e) {
      var match = $.parseJSON(e.data);
      console.log("turn ME");
      console.log(match);
      Players.updateCurrent(match, "update");
      Board.setTurn();
      Board.updateView();
    }, false);
    es.addEventListener('match_ended', function(e) {
      Match.match = $.parseJSON(e.data);
      console.log("match_ended ME");
      console.log(Match.match);
    }, false);
  },
  showInvitation : function(playerId, match) {
    if (playerId == match.player2) {
      $('#modal_invitation').foundation('reveal', 'open');
    }
  },
  rejectInvitation : function() {
    Match.end();
    $('#modal_invitation').foundation('reveal', 'close');
  },
  acceptInvitation : function() {
    if(Players.isBusy(Players.current)) {
      var match = Utils.last(Players.current.current_matches);
      Match.connect(match);
      Board.setFirstTurn(match.player2);
    }
    $('#modal_invitation').foundation('reveal', 'close');
  },
  getCurrent : function() {
    if (Players.current.current_matches.length > 0) 
      return Utils.last(Players.current.current_matches);
  }
};