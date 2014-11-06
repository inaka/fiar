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

function cleanLoginForm() {
  $("#login_username_txt").val("");
  $("#login_pass_txt").val("");
  $(".error").remove();
};

$('#modal_login .close-reveal-modal').click(function(event) {
  cleanLoginForm();
});