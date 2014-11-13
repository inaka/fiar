Fiar = {
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
    es.addEventListener('users_connected', Fiar.usersConnected, false);
    es.addEventListener('user_connected', Fiar.userConnected, false);
    es.addEventListener('user_disconnected', Fiar.userDisconnected, false);
    es.addEventListener('match_started', Fiar.matchStarted, false);
    es.addEventListener('match_ended', Fiar.matchEnded, false);

    Players.loadCurrent();
  },
  usersConnected : function(e) {
    var users = $.parseJSON(e.data);
    if (users.length > 0) {
      Players.init(users);

      Players.updateView();
      Board.toggleView();
    };
  },
  userConnected : function(e) {
    var player = $.parseJSON(e.data);
    Players.add(player);

    Players.updateView();
  },
  userDisconnected : function(e) {
    Fiar.msg = $.parseJSON(e.data);
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
    Board.toggleView();

    Match.showInvitation(Players.current.user.id, match);
  },
  matchEnded : function(e) {
    var match = $.parseJSON(e.data);
    console.log("match_ended BR");
    console.log(match);

    Players.updateCurrent(match, "end");

    Players.updateStatus(match.player1, undefined); 
    Players.updateStatus(match.player2, undefined);

    if(!Board.updateViewFinished(match)){
      Board.toggleView();
    }
    
    Players.updateView();
  }
};

$(document).ready(function() {
  if (isCookie("auth")) {
    Fiar.connect();
  };
  $("body").on('click', '#end_match_btn', Match.end);
  $("body").on('click', 'li.player.available', Match.start);
  $("body").on('click', '#accept_btn', Match.acceptInvitation);
  $("body").on('click', '#decline_btn', Match.rejectInvitation);
  $('#modal_invitation .close-reveal-modal').click(Match.acceptInvitation);
  $("body").on('click', '.reveal-modal-bg', Match.acceptInvitation);
});