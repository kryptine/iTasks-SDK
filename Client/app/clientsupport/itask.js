function __SaplHtml_handleWidgetEvent(expr,taskId,event){
	
	var state = controller.tasklets[taskId].st;
	
	// Returns a tuple of the JS document and HtmlEventResult	
	// Looks like: [0, "Tuple2", document,HtmlEventResult]	
	var ys = Sapl.feval([expr,[state,taskId,event,document]]);
	
	// The result is only in HNF, so both part of the tuple must be forced,
	// but the document can be dropped after that.
	Sapl.feval(ys[2]);
	
	ys = Sapl.feval(ys[3]);
	switch(ys[0]){
		/* [0, "KeepState"] */
		case 0: 
			// Do nothing
			break;
		/* [1, "SaveState", newstate] */
		case 1:
			var newstate = Sapl.feval(ys[2]);
			controller.tasklets[taskId].st = newstate;
			break;
		/* [2, "PersistState", newstate] */		
		case 2:
			var newstate = Sapl.feval(ys[2]);
			controller.tasklets[taskId].st = newstate;		
			controller.onEdit(taskId,"state",Sapl.toJS(newstate));
			break;
	}
}

