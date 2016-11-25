# SVG Matrix Transformations and JavaScript

No matter how much you might resist using matrix transformations with SVG documents, if you intend to modify an image dynamically (and cumulatively), matrixes are your only viable option.

Many sites tell you that you should use matrixes, ostensibly for speed purposes. In my opinion, speed is not the issue. The issue is the complexity associated with applying multiple transformations to an element; you just can't do it with simple transformations (e.g., rotate, translate, skew and/or scale).

Here are a few notes about the stumbling blocks that I encountered in my journey towards using matrix transformations:

1. Most online guides seem to assume that you will be working with a static image; they tell you how to convert simple transformations to matrixes as a one-time operation, but give you no (straightforward) information on how to subsequently alter those transformations dynamically. The JavaScript method, `element.getCTM()` is your key to handling this situation. By using this method (short for get Current Transformation Matrix), you can obtain a matrix that includes all of the transformations currently applied to your graphic element. That matrix can then be used to generate updated matrixes after applying dynamically updated transformations.
2. The `sylvester.js` JavaScript library is a great resource to handle your matrix math needs. You'll use the method `matrix.x()` to multiply the current matrix with the matrix representing the transforms you want to apply to obtain your newly combined matrix to apply to your DOM element.
3. Some guides talk about changing the `matrix.e` and `matrix.f` variables directly to apply translation transformations. When dealing with multiple transforms, that will just cause you a world of grief (e.g., rotation transforms update the e and f variables in complex ways that are difficult to calculate without matrix math).
4. Rotation transformations use `sine` and `cosine` methods extensively. At first glance, one would think that the JavaScript methods `Math.sin()` and `Math.cosin()` would work nicely. They do, but keep in mind that those methods deal in radians, not degrees. If you want to rotate something in (e.g., 45) degrees, you’ll need to convert that value to radians (e.g., 0.785398163) before using those methods.
5. You should be able to combine a translation transformation with a rotation transformation in a single matrix to choose the center of rotation, but I haven’t been able to get that to work. Instead, I perform a pre-shift to move my desired rotation point to the origin and then a post-shift to move it back after the rotation. That seems to work reliably and allows me to rotate the graphic where my pointer is hovering.

Here is an example of how I have implemented these concepts in my Flower Network Flow Analysis Visualization server (I also have some `prototype.js` markup in here and `this.nonce` refers to a random, one-time string I use to distinguish between multiple, similar, generated SVGs existing in a single DOM):

~~~~ {.javascript}
var content = $('content_' + this.nonce);
var matrix = content.getCTM();
 
var map = $('map');
 
var leftVal = map.offsetLeft;
var topVal = map.offsetTop;
var parent = map.offsetParent;
 
while(parent != null) {
    leftVal += parent.offsetLeft;
    topVal += parent.offsetTop;
    parent = parent.offsetParent;
}
 
var pointerX = event.clientX - leftVal;
var pointerY = event.clientY - topVal;
 
var radians = rotation * (Math.PI/180);
var cos = Math.cos(radians);
var sin = Math.sin(radians);
 
var current = $M([[ matrix.a, matrix.c, matrix.e ], [ matrix.b, matrix.d, matrix.f ], [0, 0, 1]]);
var preshift = $M([[ 1, 0, -pointerX], [0, 1, -pointerY], [0, 0, 1]]);
var rotated = $M([[cos, -sin, 0], [sin, cos, 0], [0, 0, 1]]);
var postshift = $M([[ 1, 0, pointerX], [0, 1, pointerY], [0, 0, 1]]);
 
var updated = postshift.x(rotated.x(preshift.x(current)));
 
content.setAttribute("transform", "matrix(" +
    updated.e(1, 1) + " " + updated.e(2, 1) + " " +
    updated.e(1, 2) + " " + updated.e(2, 2) + " " +
    updated.e(1, 3) + " " + updated.e(2, 3) + ")");
~~~~

The basic operations above are:

* Obtain the current transformation matrix. (lines 1 and 2)
* Determine how far the pointer is from the edges of the map DIV using offsetLeft and offsetTop. (lines 4-17)
* Pre-calculate the sine and cosine values of the desired rotation (convert to radians as an intermediate step). (lines 19-21)
* Convert the SVG matrix to a sylvester.js matrix ($M). (line 23)
* Build the pre-shift transformation matrix. Translation matrixes are constructed thusly, with X and Y being the number of pixels the graphic should be shifted in the X and Y directions, respectively (line 24):

~~~~ {.javascript]
[ 1, 0, X ]
[ 0, 1, Y ]
[ 0, 0, 1 ]
~~~~

* Build the rotation transformation matrix. Rotation matrixes are constructed thusly, with R being the rotation value (line 25):

~~~~ {.javascript}
[  cos(R), sin(R),   0 ]
[ -sin(R), cos(R),   0 ]
[       0,      0,   1 ]
~~~~

* Build the post-shift transformation matrix. (line 26)
* Calculate the updated matrix; note that the order of operations is important. I'm not precisely sure about the rules that apply here (I kind of guessed until I got the order right, to be perfectly honest). The really important thing to remember is that matrix multiplication is apparently sensitive to order (unlike simple multiplication). (line 28)
* Apply the updated matrix to your graphic element. Note that in my example above I’m using the `sylvester.js` (row, column) notation. The actual `matrix()` transform only uses the first six values of the full matrix - the last row of 0, 0, 1 never changes and should not be specified. (line 30-33)

By way of example, and if you have a newer version of Firefox, Chrome, Safari, Epiphany (or shockingly, even IE9!), visit a [mock-up of a generated network map here](http://justinthomas.name.s3-website-us-east-1.amazonaws.com/). _Author's Note: Now non-operational._ For anyone else, here is a still screenshot of a map that's been twisted, translated and scaled.
 
![Network Map Mockup](https://ser.endipito.us/file/networkmap.png)
 
If you do visit that page, try using your mouse wheel to rotate the map or the graphical slider to zoom in and out. You can also just drag the map around to reposition it. Clicking on a connection will open a small detail box and clicking on a node will narrow the display to only connections involving that node. Clicking on subsequent nodes will add those nodes' connections. Double clicking in the white space will cause all connections to be visible again. Hovering over a connection or node will show you that connection or node’s details (at the bottom of the graphic). The information in there is mostly nonsense - I went through and sanitized the addresses - although protocol, port and volume information are real.

I hope the above information helps someone else! I know it would have saved me a lot of time to have a working example of JavaScript code that updates a transformation matrix dynamically based on DOM events.
