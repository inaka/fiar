Utils = {
  last : function(array) {
    return array[array.length - 1];
  },
  sendRequest : function(url, method, data, success) {
    success = success ? success : function() {};
    $.ajax({
      url:url,
      type:method,
      data:data,
      contentType:"application/json",
      dataType:"json"
    })
      .done(success)
      .fail(function(xhr, textStatus) {
        alert("There was an error while sending the request: "+xhr.statusText);
      });
  }
};