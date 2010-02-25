Ext.ns("itasks.util");

itasks.util.formatDate = function(ts) {
	if (ts == null)
		return "";
	else	
		return Date.parseDate(ts, "U").format("d M Y H:i:s");
}
itasks.util.formatStartDate = function (ts) {
	if (ts == null)
		return "Not started yet";
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatDeadline = function(ts) {
	if(ts == null)
		return "No deadline";
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatPriority = function(priority) {
	switch(priority) {
		case null : return "";
		case "LowPriority": return "Low";
		case "NormalPriority": return "Normal";
		case "HighPriority": return itasks.util.coloredLabel("red","High");
	}
	return priority;
}
itasks.util.formatProgress = function(progress) {
	switch(progress) {
		case null: return "";
		case "TPActive" : return itasks.util.coloredLabel("green","Active");
		case "TPStuck" : return itasks.util.coloredLabel("purple","Stuck");
		case "TPWaiting": return itasks.util.coloredLabel("blue","Waiting");
		case "TPReject": return itasks.util.coloredLabel("red","Reject");
	}
	return progress;
}
itasks.util.coloredLabel = function (color, msg) {
	return "<span style=\"color: " + color + "; font-weight: bold;\">" + msg + "</span>";
}

//Filter method for arrays (IE does not natively support this method)
if (!Array.prototype.filter)
{
  Array.prototype.filter = function(fun /*, thisp*/)
  {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var res = new Array();
    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this)
      {
        var val = this[i]; // in case fun mutates this
        if (fun.call(thisp, val, i, this))
          res.push(val);
      }
    }

    return res;
  };
}
  
//Function to determine scrollbar width
itasks.util.getScrollerWidth =  function() {
	var scr = null;
	var inn = null;
	var wNoScroll = 0;
	var wScroll = 0;

	// Outer scrolling div
	scr = document.createElement('div');
	scr.style.position = 'absolute';
	scr.style.top = '-1000px';
	scr.style.left = '-1000px';
	scr.style.width = '100px';
	scr.style.height = '50px';
	// Start with no scrollbar
	scr.style.overflow = 'hidden';

	// Inner content div
	inn = document.createElement('div');
	inn.style.width = '100%';
	inn.style.height = '200px';

	// Put the inner div in the scrolling div
	scr.appendChild(inn);
	// Append the scrolling div to the doc
	document.body.appendChild(scr);

	// Width of the inner div sans scrollbar
	wNoScroll = inn.offsetWidth;
	// Add the scrollbar
	scr.style.overflow = 'auto';
	// Width of the inner div width scrollbar
	wScroll = inn.offsetWidth;

	// Remove the scrolling div from the doc
	document.body.removeChild(
		document.body.lastChild);

	// Pixel width of the scroller
	return (wNoScroll - wScroll);
}