Broadcast = {
  connect : function() {
    var es = new EventSource('/events');

    es.onerror = function(e){
      delete_cookie("auth");
      $('#login_pass_lbl')
          .next()
          .html('<small class="error">Username or Password incorrect.</small>');
    }
    es.onopen = function(e){
      $('#modal_login').foundation('reveal', 'close');
      cleanLoginForm();
    }
    es.addEventListener('users_connected', Broadcast.usersConnected, false);
    es.addEventListener('user_connected', Broadcast.userConnected, false);
    es.addEventListener('user_disconnected', Broadcast.userDisconnected, false);
    es.addEventListener('match_started', Broadcast.matchStarted, false);
    es.addEventListener('match_ended', Broadcast.matchEnded, false);

    Players.loadCurrent();
  },
  usersConnected : function(e) {
    var users = $.parseJSON(e.data);
    if (users.length > 0) {
      Players.init(users);

      Players.updateView();
      Board.updateView();
    };
  },
  userConnected : function(e) {
    var player = $.parseJSON(e.data);
    Players.add(player);

    Players.updateView();
  },
  userDisconnected : function(e) {
    Broadcast.msg = $.parseJSON(e.data);
    Players.updateView();
  },
  matchStarted : function(e) {
    var match = $.parseJSON(e.data);
    console.log("match_started BR");
    console.log(match);

    Players.updateCurrent(match, "start");
    
    Players.updateStatus(match.player1, match);
    Players.updateStatus(match.player2, match);

    Players.updateView();
    Board.updateView();

    Match.showInvitation(Players.current.user.id, match);
  },
  matchEnded : function(e) {
    var match = $.parseJSON(e.data);
    console.log("match_ended BR");
    console.log(match);

    Players.updateCurrent(match, "end");

    Players.updateStatus(match.player1, undefined); 
    Players.updateStatus(match.player2, undefined);

    Players.updateView();
    Board.updateView();
  }
};

/*** Turn ***/
Board = {
  enable : function() {
    $("#on_hold").hide();
    $("#board_ul").children().text("");
    $("#play_btn").removeClass("disabled");
    $("#end_match_btn").removeClass("disabled");
  },
  disable : function() {
    $("#end_match_btn").addClass("disabled");
    $("#play_btn").addClass("disabled");
    $("#board_notice").html("");
    $("#on_hold").show();
  },
  setFirstTurn : function() {
    $("#board_notice")
      .html("<p>Match started, please select a column and play!</p>");
  },
  setTurn : function() {
    $("#board_notice").html("<p>Turn.</p>");
  },
  updateView : function() {
    if(Players.isBusy(Players.current))
      Board.enable();
    else
      Board.disable();
  }
};

/* match connection */
Match = {
  start : function(event){
    var username = event.target.innerText;
    var url = "/matches";
    var method = "POST";
    var data = JSON.stringify({'player2': username});
    sendRequest(url, method, data, Match.connect);
  },
  end : function(){
    Players.current.current_matches.forEach(function (match) {
      var url = "/matches/"+match.id;
      var method = "DELETE";
      var data = "";
      sendRequest(url, method, data);
    });
  },
  connect : function(data) {
    var es = new EventSource("/matches/" + data.id + "/events");
    Board.enable();
    console.log("start listen ME");
    es.addEventListener('turn', function(e) {
      Match.msg = $.parseJSON(e.data);
      console.log("turn ME");
      console.log(Match.msg);
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
  }
}

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
    sendRequest(url, method, data, success);
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
}

Utils = {
  last : function(array) {
    return array[array.length - 1];
  }
}

/*** On load ***/
$(document).ready(function() {
  if (isCookie("auth")) {
    Broadcast.connect();
  };
  $("body").on('click', '#end_match_btn', Match.end);
  $("body").on('click', 'li.player.available', Match.start);
  $("body").on('click', '#accept_btn', Match.acceptInvitation);
  $("body").on('click', '#decline_btn', Match.rejectInvitation);
  $('#modal_invitation .close-reveal-modal').click(Match.acceptInvitation);
  $("body").on('click', '.reveal-modal-bg', Match.acceptInvitation);
});

/*** ajax request ***/
function sendRequest(url, method, data2, success) {
  success = success ? success : function() {};
  $.ajax({
    url:url,
    type:method,
    data:data2,
    contentType:"application/json",
    dataType:"json"
  })
    .done(success)
    .fail(function(xhr, textStatus) {
      alert("There was an error while sending the request: "+xhr.statusText);
    });
};