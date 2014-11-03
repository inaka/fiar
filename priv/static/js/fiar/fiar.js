Broadcast = {
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
        updatePlayers(Broadcast.msg);
      };
    }, false);
    es.addEventListener('user_conected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      if (Broadcast.current_user != Broadcast.msg.username) {
        updatePlayers(Broadcast.msg);
      };
    }, false);
    es.addEventListener('user_disconnected', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
    }, false);
    es.addEventListener('match_started', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      console.log("match_started BR");
      console.log(Broadcast.msg);
    }, false);
    es.addEventListener('match_ended', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      console.log("match_ended BR");
      console.log(Broadcast.msg);
    }, false);
  },
  setMe : function(data){
    Broadcast.current_user = data.username;
  }
};

function updatePlayers(msg) {
  if (msg.length > 0) {
    $("ul#players_online").html("");
    Object.keys(Broadcast.msg).forEach(function (key) {
      updatePlayer(msg[key].user);
    });
  }else{
    $("#"+msg.username).parent().remove();
    updatePlayer(msg);
  }
};

function updatePlayer(msg){
  if (msg != "unregistered" && msg.current_matches == undefined) {
    $("ul#players_online").append( "<li><a id='"+msg.username+"' class='player' href='#'>"
                                   + msg.username
                                   + "</a></li>"
                                 );
  } else {
    $("ul#players_online").append( "<li id='"+msg.username+"' class='busy'>"
                                   + msg.username
                                   + "</li>"
                                 );
  };
};

$( document ).ready(function() {
  if (isCookie("auth")) {
    Broadcast.setConnection();
    var url = "/me";
    var method = "GET";
    var data = "";
    sendRequest(url, method, data);
  };
});

$("body").on('click', '#end_match_btn', function () {
  MatchConnection.endMatch();
});

$("body").on('click', '.player', function () {
  var username = event.target.innerText;
  var url = "/matches";
  var method = "POST";
  var data = JSON.stringify({'player2': username});
  sendRequest(url, method, data);
});

MatchConnection = {
  endMatch : function(){
    if (Mid != undefined) {
      var url = "/matches/"+ Mid;
      var method = "DELETE";
      var data = "";
      sendRequest(url, method, data);
    };
  },
  startMatchOk : function(data) {
    var es = new EventSource("/matches/" + data.id + "/events");
    es.addEventListener('turn', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
      console.log("turn");
      console.log(Broadcast.msg);
    }, false);
    es.addEventListener('match_ended', function(e) {
      Broadcast.msg = $.parseJSON(e.data);
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
      Broadcast.setMe(data);
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