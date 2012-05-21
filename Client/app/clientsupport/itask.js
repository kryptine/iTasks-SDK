/**
* eventType can be "edit" or "commit" or "init". It is necessary because eventValue can be null
* even in the case of "edit" event.
*/
function controllerWrapper(taskletId,controllerFunc,taskId,eventType,eventName,eventValue){

	/*
	var splitted = taskId.split("-");
	var saplTaskId = [0, 'TaskId', [splitted[0], splitted[0]]];
	*/
	
	var state = controller.tasklets[taskletId].st;

	eval("var tmp = eval([" + controllerFunc + ",[]]);");
	tmp[1].push(taskId);
	tmp[1].push(state);
	
	if(eventType == "edit" || eventType == "commit"){
		tmp[1].push([1, 'Just', eventName])
	}else{
		tmp[1].push([0, 'Nothing']);
	}

	if(eventType == "edit"){
		// convert eventValue to JSON to mediate type information to the controller (e.g. Int?)
		tmp[1].push([1, 'Just', JSON.stringify(eventValue)])
	}else{
		tmp[1].push([0, 'Nothing']);
	}	

	// result is a tuple of mbTUI, command and state
	var ys = Sapl.feval(tmp);
	state = Sapl.feval(ys[4]);
	controller.tasklets[taskletId].st = state;	// save it
	var cmd = Sapl.feval(ys[3]);
	
	var mbTUI = Sapl.feval(ys[2]);	
	if(mbTUI[0] == 0){
		alert("Nothing");
	}else{
		var tuistr = Sapl.feval(mbTUI[2]);
		eval("var tui = " + tuistr + ";");
		applytui(controller.tasklets[taskletId].tui, tui);
		//alert(tui);
	}
}

function applytui(widget,tui){
	
/*	
	if(tui.xtype === "itasks_widget_placeholder"){
		if(widget.xtype === "itasks_html"){
			
			if(tui.tui != null){
				applytui(widget.tui,tui.tui);
			}
			return;
		}
	}
*/

	if(tui.xtype !== widget.xtype){
		throw "ERROR: TUI/Object structure doesn't match!";
		return;
	}
			
	for(var prop in tui) {
		if(tui.hasOwnProperty(prop)){

			var val = tui[prop];
			if(prop === "xtype") continue;
			if(prop === "value") continue;
				
			if(isString(val) || isBoolean(val) || isNumber(val)){
				
				if(widget[prop] !== val){
					console.log("set "+widget.xtype+" property \""+prop+"\" to \""+val+"\"");
					
					var setter = "widget.set"+prop.capitalize();
					
					if (eval("typeof " + setter + " == 'function'")) {
					
						console.log("...done");
						
						// instead of widget[prop] = val; to fire change event
						if(isString(val)){
							eval(setter+"(\""+val+"\");");
						}else{
							eval(setter+"("+val+");");
						}
					}else{
						console.log("...setter is not found");
					}
				}
				
			}else if(isArray(val)){
				
				try{				
				
					for(var i=0;i<val.length;i++){
					
						// ExtJS :(
						if(prop === "items"){
							var children = widget.items ? widget.items.items : [];
							applytui(children[i],val[i]);
						}else{
							applytui(widget[prop][i],val[i]);
						}
						
					}
				
				}catch(e){
					// Fallback to replace. TODO: works for "items" only
					widget.removeAll();
					for(var i=0;i<val.length;i++) widget.add(val[i]);
				}				
			}else{
				
				console.error("Unhandled type for property \""+prop+"\"");
			}
			
		}			
	}	
}

function __SaplHtml_handleJSEvent(expr,taskId,event){
	
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
