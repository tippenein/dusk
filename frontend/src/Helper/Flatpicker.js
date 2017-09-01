exports.flatpicker = function(el){
    return function() {
        return $(el).flatpickr(
            {enableTime: true,
             minDate: "today",
             altInput: true,
            })
   }
}
