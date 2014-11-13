Board = {
  enable : function() {
    $("#on_hold").hide();
    Board._clean();
    $("#board_ul").removeClass("show-board");
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
  setTurn : function(playerTurnId) {
    $("#board_notice").html("");
    if (!Board.updateViewFinished(Match.getCurrent())) {
      currentPlayer = Players.current;
      if (currentPlayer.user.id == playerTurnId){
        $("#board_notice").append("<p>your turn.</p>").show("slow");
      }
    };
  },
  toggleView : function() {
    if(Match.getCurrent()){
      Board.enable();
      Board.updateView();
    } else {
      Board.disable();
    }
  },
  updateViewFinished : function(match) {
    if (match.status == "won_by_player2") {
      Board.updateViewWon(match.player1, match.player2, 2);
      return true;
    }else if(match.status == "won_by_player1"){
      Board.updateViewWon(match.player1, match.player2, 1);
      return true;
    }else if(match.status == "drawn"){
      Board.updateViewDrawn(match.player1, match.player2);
      return true;
    }else{
      return false;
    };
  },
  updateViewWon : function(playerId1, playerId2, playerWon) {
    var currentId = Players.current.user.id;
    if (currentId == playerId1 || currentId == playerId2) {
      Board.disable();
      $("#board_ul").addClass("show-board");
      $("#board_notice").addClass('won-notification')
                        .text("Player " + playerWon + " won.");
    };
  },
  updateViewDrawn : function(playerId1, playerId2) {
    var currentId = Players.current.user.id;
    if (currentId == playerId1 || currentId == playerId2) {
      $("#end_match_btn").addClass("disabled");
      $("#play_btn").addClass("disabled");
      $("#board_notice").html("Match drawn.");
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
    $("#board_notice").html("");
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