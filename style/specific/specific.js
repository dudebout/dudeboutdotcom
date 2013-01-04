!function( $ ){

  $.fn.myPlugin = function(butid) {

    // there's no need to do $(this) because
    // "this" is already a jquery object

    // $(this) would be the same as $($('#element'));
    $(butid).attr("disabled", "disabled");
    this.collapse('toggle');
    this.on('shown', function () {
        $(butid).addClass('active');
        $(butid).removeAttr("disabled");
    });
    this.on('hidden', function () {
        $(butid).removeClass('active');
        $(butid).removeAttr("disabled");
    });
    // this.fadeIn('normal', function(){

    //   // the this keyword is a DOM element

    // });

  };
}( jQuery );
