/* global exports */
"use strict";


exports.fileUpload = function(selector, url){
    return function(){
        var formData = new FormData();

        var el = document.getElementById(selector);
        if (el.files.length > 0) {
            formData.append(selector, el.files[0]);

            var request = new XMLHttpRequest();
            request.open("POST", url);
            request.send(formData);
        }
        return null;
    };
};
