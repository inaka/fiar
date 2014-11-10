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

$("#columns .col").click(function(event){
  $("#columns .col").removeClass("selected");
  $(this).addClass("selected");
})