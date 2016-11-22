# CSS Tables with Fixed Headers

I’ve been using a table in one of my web applications for a while that’s bothered me. The table displays NetFlow data and may contain many thousands of lines with fields of varying lengths (e.g., shorter fields for IPv4 addresses and longer fields for IPv6 addresses). To allow for maximum flexibility, I’ve defined the table using percentages so that it can be expanded to full screen or to occupy a smaller window.

The aspect that bothered me the most was the header. To get the labels to line up correctly, the header needed to be part of the larger table to accommodate the flexible field widths; if I wanted the labels to line up, it could not be defined as an outer table with an inner div that allowed for scrolling as many sites suggest. This meant that the header would scroll off the page when the data was accessed. However, I finally figured out a way to overcome that difficulty.

I use a lot of JavaScript in this application, so relying on that to resize fields doesn’t give me much heartache. I figured out that I can query the inner elements for their offsetWidth after they’ve been added to the table and then dynamically style the header field widths to match. Below is my example.

Given this structure:

~~~~ {.html}
<table class="flows">
 
  <thead id="flow-header">
    <tr>
      <th id="start">Start Time</th>
      <th id="protocol">Type</th>
      <th id="source">Source Address</th>
      <th id="sport">Port</th>
      <th id="destination">Destination Address</th>
      <th id="dport">Port</th>
      <th id="flags">Flags</th>
      <th id="size">Size</th>
    </tr>
  </thead>
 
  <tfoot id="flow-footer">
    <tr>
      <td>Start Time</td>
      <td>Type</td>
      <td>Source Address</td>
      <td>Port</td>
      <td>Destination Address</td>
      <td>Port</td>
      <td>Flags</td>
      <td>Size</td>
    </tr>
  </tfoot>
 
  <tbody>
    <tr>
      <td colspan="8">
        <div id="flow-table">
          <table id="flow-data"></table>
        </div>
      </td>
    </tr>
  </tbody>
 
</table>
~~~~

. . . I can use this JavaScript snippet to resize the th fields (and by extension the footer td fields) to match the data that is pulled in via AJAX to the “flow-data” table:

~~~~ {.javascript}
$('start').style.width = $('flow-data').childNodes[0].childNodes[0].offsetWidth + "px";
$('protocol').style.width = $('flow-data').childNodes[0].childNodes[1].offsetWidth + "px";
$('source').style.width = $('flow-data').childNodes[0].childNodes[2].offsetWidth + "px";
$('sport').style.width = $('flow-data').childNodes[0].childNodes[3].offsetWidth + "px";
$('destination').style.width = $('flow-data').childNodes[0].childNodes[4].offsetWidth + "px";
$('dport').style.width = $('flow-data').childNodes[0].childNodes[5].offsetWidth + "px";
$('flags').style.width = $('flow-data').childNodes[0].childNodes[6].offsetWidth + "px";
$('size').style.width = $('flow-data').childNodes[0].childNodes[7].offsetWidth + "px";
~~~~

Obviously, I use prototype.js to make my life a little easier, but that’s the extent of the JavaScript frameworks that I employ.

The result is a table wherein the header fields are dynamically resized to match the content that is pulled in to the DOM (gradually – as the scrollbar moves down the table).

I’ll probably get rid of all those ID tags, since I should be able to refer to them relative from their parent elements.
