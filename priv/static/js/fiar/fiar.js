Broadcast = {
  busy_players : [],
  setConnection : function(){
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
    es.addEventListener('users_conected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      if (Broadcast.msg != "unregistered" && Broadcast.msg.length > 0) {
        fillBusyList(Broadcast.msg);
        updatePlayers(Broadcast.msg);
        enableBoard(Broadcast.current_user);
      };
    }, false);
    es.addEventListener('user_conected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      if (Broadcast.current_user.user.username != Broadcast.msg.user.username) {
        updateBusyPlayer(Broadcast.msg);
        updatePlayers(Broadcast.msg);
      };
    }, false);
    es.addEventListener('user_disconnected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
    }, false);
    es.addEventListener('match_started', function(e) {
      var match = $.parseJSON(e.data);
      console.log("match_started BR");
      console.log(match);
      updateCurrentUser(match);
      updateBusyPlayer(match.player1);
      updateBusyPlayer(match.player2);
      setAsBusy(Broadcast.busy_players);
      enableBoard(Broadcast.current_user);
      if (Broadcast.current_user.user.id == match.player2) {
        openInvitationModal();
      };
    }, false);
    es.addEventListener('match_ended', function(e) {
      var match = $.parseJSON(e.data);
      console.log("match_ended BR");
      console.log(match);
      updateCurrentUser(match);
      updateBusyPlayer(match.player1);
      updateBusyPlayer(match.player2);
      setAsFree([match.player1, match.player2]);
      setOnHold();
    }, false);
    getCurrentUser();
  }
};

/**** Invitation ****/
function openInvitationModal() {
  $('#modal_invitation').foundation('reveal', 'open');
}

function closeInvitationModal() {
  $('#modal_invitation').foundation('reveal', 'close');
}

function invitationAccepted(){
  var lastMatchPos = Broadcast.current_user.current_matches.length - 1;
  Match.setConnection(Broadcast.current_user.current_matches[lastMatchPos]);
  setFirstTurn(Broadcast.current_user.current_matches[lastMatchPos].player2);  
}

$("body").on('click', '#accept_btn', function () {
  invitationAccepted();
  closeInvitationModal();
});

$("body").on('click', '#decline_btn', function () {
  Match.endMatch();
  closeInvitationModal();
});

$('#modal_invitation .close-reveal-modal').click(function(event) {
  invitationAccepted();
});

$("body").on('click', '.reveal-modal-bg', function () {
  invitationAccepted();
});

/*** Turn ***/
function enableBoard(current_user){
  if (isBusy(current_user.user.id)) {
    $("#on_hold").hide();
    $("#board_ul").children().text("");
    $("#play_btn").removeClass("disabled");
    $("#end_match_btn").removeClass("disabled");
  };
}

function setOnHold(){
  $("#end_match_btn").addClass("disabled");
  $("#play_btn").addClass("disabled");
  $("#board_notice").html("");
  $("#on_hold").show();
}

function setFirstTurn(playerId){
  if (playerId == Broadcast.current_user.user.id) {
    $("#board_notice").append("<p>Match started, please select a column and play!</p>");
  };
}

/* match connection */
Match = {
  endMatch : function(){
    if (Broadcast.current_user.current_matches != undefined) {
      Broadcast.current_user.current_matches.forEach(function (match) {
        var url = "/matches/"+match.id;
        var method = "DELETE";
        var data = "";
        sendRequest(url, method, data);
      });
    };
  },
  setConnection : function(data) {
    var es = new EventSource("/matches/" + data.id + "/events");
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
  }
}

/*** On load ***/
$( document ).ready(function() {
  if (isCookie("auth")) {
    Broadcast.setConnection();
  };
});

$("body").on('click', '#end_match_btn', function () {
  Match.endMatch();
});

$("body").on('click', '.player a', function (event) {
  if (Broadcast.current_user.user.username != event.target.innerText) {
    var username = event.target.innerText;
    var url = "/matches";
    var method = "POST";
    var data = JSON.stringify({'player2': username});
    sendRequest(url, method, data);
  };
});

/*** Current user ***/
function getCurrentUser(){
  var url = "/me";
  var method = "GET";
  var data = "";
  sendRequest(url, method, data);
};

function setCurrentUser(data){
  Broadcast.current_user = data;
};

function updateCurrentUser(match){
  if (Broadcast.current_user.user.id == match.player1 ||
      Broadcast.current_user.user.id == match.player2) {
    if (Broadcast.current_user.current_matches == undefined ||
        Broadcast.current_user.current_matches.length < 1) {
      Broadcast.current_user.current_matches = [match];
    }else{
      delete Broadcast.current_user.current_matches;
    }
  };
}

/*** Busy list ***/
function fillBusyList(players){
  players.forEach(function (player) {
    updateBusyPlayer(player); 
  });
}

function updateBusyPlayer(player){
  playerId = player.user.id;
  if (player.current_matches == undefined || player.current_matches.length > 0) {
    Broadcast.busy_players.push(playerId);
  }else{
    var index = Broadcast.busy_players.indexOf(playerId);
    if (index != -1) {
      Broadcast.busy_players.splice(index, 1);
    }else{
      console.log("busy player not added to list");
    };
  }
};

function isBusy(playerId){
  var found = jQuery.inArray(playerId, Broadcast.busy_players);
  if (found >= 0) {
      return true;
  } else {
      return false;
  };
};

function setAsBusy(ids) {
  ids.forEach(function (id){
    var username = $('#player_'+id+' a').text();
    $('#player_'+id).html( "<li id='player_"
                         + id
                         + "' class='player busy'>"
                         + username
                         + "</li>");
  });
};
function setAsFree(ids) {
  ids.forEach(function (id){
    var username = $('#player_'+id).text();
    $('#player_'+id).html( "<li id='player_"
                         + id
                         + "' class='player'><a href='#'>"
                         + username
                         + "</a></li>");
  });
};

/*** Update list ***/
function updatePlayers(msg) {
  console.log(msg);
  if (msg.length > 0) {
    $("ul#players_online").html("");
    Object.keys(msg).forEach(function (key) {
      if (msg[key].current_matches != undefined) {
        if (msg[key].current_matches.length > 0) {
          updatePlayer(msg[key].user, true);
        } else {
          updatePlayer(msg[key].user, isBusy(msg.id));
        }
      }
    });
  }else{
    $("#player_"+msg.user.id).remove();
    updatePlayer(msg.user, isBusy(msg.user.id));
  }
};

function updatePlayer(msg, match){
  if (match) {
    $("ul#players_online").append( "<li id='player_"
                                 + msg.id
                                 + "' class='player busy'>"
                                 + msg.username
                                 + "</li>"
                                 );
  }else{
    $("ul#players_online").append( "<li id='player_"
                                 + msg.id 
                                 + "' class='player'><a href='#'>"
                                 + msg.username
                                 + "</a></li>"
                                 );
  };
};

/*** ajax request ***/
function sendRequest(url, method, data2) {
  $.ajax({
    url:url,
    type:method,
    data:data2,
    contentType:"application/json",
    dataType:"json"
  }).done(function(data, textStatus, xhr) {
    if(url == "/matches" && method == "POST" && xhr.status == 200) {
      Match.setConnection(data);
    } else if(url == "/me" && method == "GET" && xhr.status == 200) {
      setCurrentUser(data);
    };
  }).fail(function(xhr, textStatus) {
    if(url == "/matches" && method == "POST" && xhr.status == 409) {
      alert("Could not start the game :( "+xhr.statusText);
    };
  });
};