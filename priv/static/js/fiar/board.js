Board = {
  enable : function() {
    $("#on_hold").hide();
    Board._clean();
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
    console.log("entra");
    $("#board_notice").fadeToggle( "slow", "linear", function(){
      $(this).append("your turn.");
    });
  },
  toggleView : function() {
    if(Match.getCurrent()){
      Board.enable();
      Board.updateView();
    } else {
      Board.disable();
    }
  },
  updateView : function() {
    Board._clean();
    var board = Match.getCurrent().state.board;
    $.each(board, function(i, col){
      $.each(col.reverse(), function(j, chip){
        var colId = "#" + (j + 1) + "_" + (i + 1);
        $(colId).html(chip);
      });
    });
  },
  selectColumn : function() {
    $("#columns .col").removeClass("selected");
    $(this).addClass("selected");
  },
  play : function() {
    var columnSelected = $("#columns .col.selected").attr("data-id");
    var matchId = Match.getCurrent().id;
    var url = "/matches/" + matchId;
    var method = "PUT";
    var data = JSON.stringify({'column': parseInt(columnSelected)});
    var success = Board._postPlay;
    var fail = Board._invalidPlay;
    Utils.sendRequest(url, method, data, success, fail);
  },
  _postPlay : function(match) {
    Players.updateCurrent(match, "update");
    Board.updateView(match);
  },
  _invalidPlay : function(){
    alert("Still is not your turn.");
  },
  _clean : function(){
    $("#board_ul").children().text("-");
  }
};

$("#columns .col").click(Board.selectColumn);
$("#play_btn").click(Board.play);
