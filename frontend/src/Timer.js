/* global exports */
"use strict";


// module Control.Monad.Eff.Timer

exports.timeout = function(time){
    return function(fn){
        return function(){
            return setTimeout(fn, time);
        };
    };
};
