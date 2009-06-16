Ext.ns('itasks.util');

itasks.util.formatDate = function( timestamp) {
	if (timestamp == null)
		return "";
		
	return Date.parseDate(timestamp, "U").format("d M Y H:i");
}
itasks.util.formatPriority = function(priority) {
	switch(priority) {
		case null : return "";
		case "LowPriority": return '<span style="color: green; font-weight: bold">Low</span>';
		case "NormalPriority": return '<span style="color: orange; font-weight: bold">Normal</span>';
		case "HighPriority": return '<span style="color: red; font-weight: bold">High</span>';
	}
	return priority;
}