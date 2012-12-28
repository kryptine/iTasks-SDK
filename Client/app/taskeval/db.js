DB = new function () {
	
	this.saveValue = function(iid, name, val){
		if(val){
			localStorage.setItem(iid+"-"+name, val);
		}else{
			localStorage.removeItem(iid+"-"+name);
		}
	}
	
	this.loadValue = function(iid, name){
		return localStorage.getItem(iid+"-"+name);
	}	

	this.removeValue = function(iid, name){
		return localStorage.removeItem(iid+"-"+name);
	}	
	
	this.saveTasklet = function(tasklet){
	
		var iid = tasklet.iid;
		
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		var events = DB.stringify(tasklet.events);
		var resultFunc = DB.stringify(tasklet.resultFunc);
		var tui = JSON.stringify(tasklet.tui);
		var html = tasklet.html;
		var controllerFunc = DB.stringify(tasklet.controllerFunc);
		var instanceNo = tasklet.instanceNo;
		
		this.saveValue(iid, "st", st);
		this.saveValue(iid, "events", events);
		this.saveValue(iid, "resultFunc", resultFunc);
		this.saveValue(iid, "tui", tui);
		this.saveValue(iid, "html", html);
		this.saveValue(iid, "controllerFunc", controllerFunc);
		this.saveValue(iid, "instanceNo", instanceNo);
		this.saveValue(iid, "width", tasklet.width);
		this.saveValue(iid, "height", tasklet.height);
	}
	
	this.stringify = function(o){
		if(o == null){
			return null;
		}else{
			return Sapl.dynamicToString(o);
		}
	}
		
	this.loadTasklet = function(iid, o){
		o.st = this.loadValue(iid, "st");
		o.events = JSON.parse(this.loadValue(iid, "events"));
		o.resultFunc = this.loadValue(iid, "resultFunc");
		o.html = this.loadValue(iid, "html");
		o.tui = JSON.parse(this.loadValue(iid, "tui"));
		o.controllerFunc = this.loadValue(iid, "controllerFunc");
		o.instanceNo = this.loadValue(iid, "instanceNo");
		o.width = parseInt(this.loadValue(iid, "width"));
		o.height = parseInt(this.loadValue(iid, "height"));
	}
	
	this.updateTasklet = function(tasklet, html, tuistr){
	
		var iid = tasklet.iid;	
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		
		this.saveValue(iid, "st", st);
		this.saveValue(iid, "tui", tuistr);
		this.saveValue(iid, "html", html);	
	}
	
	this.removeTasklet = function(iid){
		this.removeValue(iid, "st");
		this.removeValue(iid, "events");
		this.removeValue(iid, "resultFunc");
		this.removeValue(iid, "tui");
		this.removeValue(iid, "html");
		this.removeValue(iid, "controllerFunc");
		this.removeValue(iid, "instanceNo");
		this.removeValue(iid, "width");
		this.removeValue(iid, "height");			
	}
}

