Ext.ns('itasks.util');

itasks.util.formatDate = function( timestamp) {
	return Date.parseDate(timestamp, "U").format("d M Y");
}
itasks.util.formatPriority = function(priority) {
	switch(priority) {
		case "LowPriority": return '<span style="color: green">Low</span>';
		case "NormalPriority": return '<span style="color: orange">Normal</span>';
		case "HighPriority": return '<span style="color: red">High</span>';
	}
	return priority;
}

function void() {

}