$(document).ready(function() {
    $(window).on('scroll', function() {
        var fromTop = $(window).scrollTop();
        $('body').toggleClass('down', (fromTop > 100));
        console.log("scrolling: " + fromTop)
    });

    if (window.location.pathname == $('#next').attr('href')) {
        $('#next').toggleClass('not-active')
    } else if (window.location.pathname == $('#previous').attr('href')) {
        $('#previous').toggleClass('not-active')
    }
});
