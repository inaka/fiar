function setCookie(cvalue) {
  document.cookie = "auth=" + cvalue + ";";
};

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1);
        if (c.indexOf(name) != -1) return c.substring(name.length, c.length);
    }
    return "";
};

function checkCookie(cname, cvalue) {
    var result = getCookie(cname);
    if (result == cvalue) {
        return true;
    } else {
        return false;
    }
};

function isCookie(cname) {
    var result = getCookie(cname);
    if (result != "") {
        return true;
    } else {
        return false;
    }
};

function delete_cookie(cname) {
  document.cookie = cname + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
};