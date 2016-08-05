$(document).ready(function() {
    $(window).on("scroll", function() {
        var fromTop = $("body").scrollTop();
        $('body').toggleClass("down", (fromTop > 100));
    });
});
