$(document).ready(function() {
    $(window).on('scroll', function() {
        var fromTop = $('body').scrollTop();
        $('body').toggleClass('down', (fromTop > 100));
    });

    if (window.location.pathname == $('#next').attr('href')) {
        $('#next').toggleClass('not-active')
    } else if (window.location.pathname == $('#previous').attr('href')) {
        $('#previous').toggleClass('not-active')
    }

    // This is called with the results from from FB.getLoginStatus().
    window.statusChangeCallback = function(response) {
        console.log('statusChangeCallback');
        console.log(response);
        // The response object is returned with a status field that lets the
        // app know the current login status of the person.
        // Full docs on the response object can be found in the documentation
        // for FB.getLoginStatus().
        if (response.status === 'connected') {
            // Logged into your app and Facebook.
            window.testAPI();
        } else if (response.status === 'not_authorized') {
            // The person is logged into Facebook, but not your app.
            document.getElementById('status').innerHTML = 'Please log ' +
                'into this app.';
        } else {
            // The person is not logged into Facebook, so we're not sure if
            // they are logged into this app or not.
            document.getElementById('status').innerHTML = 'Please log ' +
                'into Facebook.';
        }
    }

    // This function is called when someone finishes with the Login
    // Button.  See the onlogin handler attached to it in the sample
    // code below.
    window.checkLoginState = function() {
        FB.getLoginStatus(function(response) {
            window.statusChangeCallback(response);
        });
    }

    window.fbAsyncInit = function() {
        FB.init({
            appId      : '1274411055939724',
            cookie     : true,
            xfbml      : true,
            version    : 'v2.8'
        });

        FB.getLoginStatus(function(response) {
            window.statusChangeCallback(response);
        });
        
        FB.AppEvents.logPageView();
    };

    (function(d, s, id){
        var js, fjs = d.getElementsByTagName(s)[0];
        if (d.getElementById(id)) {return;}
        js = d.createElement(s); js.id = id;
        js.src = "//connect.facebook.net/en_US/sdk.js";
        fjs.parentNode.insertBefore(js, fjs);
    }(document, 'script', 'facebook-jssdk'));

    // Here we run a very simple test of the Graph API after login is
    // successful.  See statusChangeCallback() for when this call is made.
    window.testAPI = function () {
        console.log('Welcome!  Fetching your information.... ');
        FB.api('/me', function(response) {
            console.log('Successful login for: ' + response.name);
            document.getElementById('status').innerHTML =
                'Thanks for logging in, ' + response.name + '!';
        });
    }
});
