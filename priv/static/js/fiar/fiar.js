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
      };
    }, false);
    es.addEventListener('user_conected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      if (Broadcast.current_user.user.username != Broadcast.msg.username) {
        console.log("user_connected");

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
      addBusyPlayer(match.player1);      
      addBusyPlayer(match.player2);      
      setAsBusy(Broadcast.busy_players);
    }, false);
    es.addEventListener('match_ended', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      console.log("match_ended BR");
      console.log(Broadcast.msg);
    }, false);
    getCurrentUser();
  }
};

function fillBusyList(players){
  players.forEach(function (player) {
    if (player.current_matches != undefined) {
      if (player.current_matches.length > 0) {
        addBusyPlayer(player.user.id); 
      }
    }
  });
}

function addBusyPlayer(playerId){
  if (!isBusy(playerId)) {
    Broadcast.busy_players.push(playerId);
  }
  console.log("busy");
  console.log(Broadcast.busy_players);
};

function isBusy(playerId){
  var found = jQuery.inArray(playerId, Broadcast.busy_players);
  if (found >= 0) {
      return true;
  } else {
      return false;
  };
};

function setCurrentUser(data){
  Broadcast.current_user = data;
};

function setAsBusy(ids) {
  ids.forEach(function (id){
    console.log(id);
    var username = $('#player_'+id+' a').text();
    $('#player_'+id).html("<li id='player_"+id+"' class='player busy'>"+username+"</li>");
  });
};

function getCurrentUser(){
  var url = "/me";
  var method = "GET";
  var data = "";
  sendRequest(url, method, data);
};

function updatePlayers(msg) {
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
    $("#player_"+msg.id).remove();
    updatePlayer(msg, isBusy(msg.id));
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
    console.log(msg);
    console.log(match);
    $("ul#players_online").append( "<li id='player_"
                                 + msg.id 
                                 + "' class='player'><a href='#'>"
                                 + msg.username
                                 + "</a></li>"
                                 );
  };
};

$( document ).ready(function() {
  if (isCookie("auth")) {
    Broadcast.setConnection();
  };
});

$("body").on('click', '#end_match_btn', function () {
  MatchConnection.endMatch();
});

$("body").on('click', '.player a', function () {
  var username = event.target.innerText;
  var url = "/matches";
  var method = "POST";
  var data = JSON.stringify({'player2': username});
  sendRequest(url, method, data);
});

MatchConnection = {
  endMatch : function(){
    console.log(Broadcast.current_user.current_matches);
    if (Broadcast.current_user.current_matches != undefined) {
      Broadcast.current_user.current_matches.forEach(function (match) {
        var url = "/matches/"+match.id;
        var method = "DELETE";
        var data = "";
        sendRequest(url, method, data);
      });
    };
  },
  startMatchOk : function(data) {
    var es = new EventSource("/matches/" + data.id + "/events");
    es.addEventListener('turn', function(e) {
      // Broadcast.msg = $.parseJSON(e.data);
      console.log("turn");
      console.log(Broadcast.msg);
    }, false);
    es.addEventListener('match_ended', function(e) {
      // Broadcast.msg = $.parseJSON(e.data);
      console.log("match_ended");
      console.log(Broadcast.msg);
    }, false);
    
  },
  endMatchOk : function(){
  }
}

$('#register_btn').click(function(event) {
  event.preventDefault();
  var username = $("#register_username_txt").val();
  var pass = $("#register_pass_txt").val();
  var data = JSON.stringify({'username': username, 'pass':pass});
  $.ajax({
    url:'/users',
    type:"POST",
    data:data,
    contentType:"application/json",
    dataType:"json"
  }).done(function(data, textStatus, xhr) {
    if (xhr.status == 200) {
      $('#modal_register').foundation('reveal', 'close');
      $('#modal_success').foundation('reveal', 'open');
    };
  }).fail(function(xhr, textStatus) {
    if (xhr.status == 409) {
      $('#register_username_lbl')
        .next()
        .html('<small class="error">Invalid entry</small>');
    };
  });
});

$('#login_btn').click(function(event) {
  username = $("#login_username_txt").val();
  pass = $("#login_pass_txt").val();
  cvalue = btoa(username + ":" + pass);
  if (!checkCookie("auth", cvalue)) {
    setCookie(cvalue);
    Broadcast.setConnection();
  } else {
    $('#modal_login').foundation('reveal', 'close');
  };
});

function sendRequest(url, method, data2) {
  $.ajax({
    url:url,
    type:method,
    data:data2,
    contentType:"application/json",
    dataType:"json"
  }).done(function(data, textStatus, xhr) {
    if (method == "DELETE" && xhr.status == 204) {
      MatchConnection.endMatchOk();
    } else if(url == "/matches" && method == "POST" && xhr.status == 200) {
      MatchConnection.startMatchOk(data);
    } else if(url == "/me" && method == "GET" && xhr.status == 200) {
      setCurrentUser(data);
    };
  }).fail(function(xhr, textStatus) {
    if(url == "/matches" && method == "POST" && xhr.status == 409) {
      alert("Could not start the game :( "+xhr.statusText);
    };
  });
};

function cleanLoginForm() {
  $("#login_username_txt").val("");
  $("#login_pass_txt").val("");
  $(".error").remove();
};

$('.close-reveal-modal').click(function(event) {
  cleanLoginForm();
});