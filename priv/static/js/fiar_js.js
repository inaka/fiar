$( document ).ready(function() {
    if (isCookie("auth")) {
      setConnection();
    };
  });

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
      })

$('#login_btn').click(function(event) {
  username = $("#login_username_txt").val();
  pass = $("#login_pass_txt").val();
  cvalue = btoa(username + ":" + pass);
  if (!checkCookie("auth", cvalue)) {
    setCookie(cvalue);
    setConnection();
  } else {
    $('#modal_login').foundation('reveal', 'close');
  };
})

function setConnection(){
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
  es.onmessage = function(e) {
    var msg = $.parseJSON(e.data);
    uploadPlayersOnline(msg);
  }
  return es;
}

function setCookie(cvalue) {
  document.cookie = "auth=" + cvalue + ";";
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1);
        if (c.indexOf(name) != -1) return c.substring(name.length, c.length);
    }
    return "";
}

function checkCookie(cname, cvalue) {
    var result = getCookie(cname);
    if (result == cvalue) {
        return true;
    } else {
        return false;
    }
}

function isCookie(cname) {
    var result = getCookie(cname);
    if (result != "") {
        return true;
    } else {
        return false;
    }
}

function delete_cookie(cname) {
  document.cookie = cname + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
}

function uploadPlayersOnline(msg) {
  $("ul#players_online").html("");
  Object.keys(msg).forEach(function (key) {
    if (msg[key].current_matches.length == 0) {
      $("ul#players_online").append( "<li><a href='#'>"
                                     + msg[key].user.username
                                     + "</a></li>"
                                     );
    } else {
      $("ul#players_online").append( "<li class='busy'>"
                                     + msg[key].user.username
                                     + "</li>"
                                     );
    };
  });
}

function cleanLoginForm() {
  $("#login_username_txt").val("");
  $("#login_pass_txt").val("");
  $(".error").remove();
}

$('.close-reveal-modal').click(function(event) {
  cleanLoginForm();
})