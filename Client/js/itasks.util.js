Ext.ns('itasks.util');

itasks.util.formatDate = function(ts) {
	if (ts == null)
		return "";
	else	
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatStartDate = function (ts) {
	if (ts == null)
		return "Not started yet"
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatDeadline = function(ts) {
	if(ts == null)
		return "No deadline"
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatPriority = function(priority) {
	switch(priority) {
		case null : return "";
		case "LowPriority": return itasks.util.coloredLabel("green","Low");
		case "NormalPriority": return itasks.util.coloredLabel("orange","Normal");
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