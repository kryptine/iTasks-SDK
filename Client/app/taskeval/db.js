"use strict";

var DB = new function () {
	
	this.saveValue = function(taskId, name, val){
		if(val){
			localStorage.setItem(taskId+"-"+name, val);
		}else{
			localStorage.removeItem(taskId+"-"+name);
		}
	}
	
	this.loadValue = function(taskId, name){
		return localStorage.getItem(taskId+"-"+name);
	}	

	this.removeValue = function(taskId, name){
		return localStorage.removeItem(taskId+"-"+name);
	}	
	
	this.saveTasklet = function(tasklet){
	
		var taskId = tasklet.taskId;
		
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		var events = DB.stringify(tasklet.events);
		var resultFunc = DB.stringify(tasklet.resultFunc);
		var updateFunc = DB.stringify(tasklet.updateFunc);
		var tui = JSON.stringify(tasklet.tui);
		var html = tasklet.html;
		var controllerFunc = DB.stringify(tasklet.controllerFunc);
		var instanceNo = tasklet.instanceNo;
		
		this.saveValue(taskId, "st", st);
		this.saveValue(taskId, "events", events);
		this.saveValue(taskId, "resultFunc", resultFunc);
		this.saveValue(taskId, "updateFunc", updateFunc);
		this.saveValue(taskId, "tui", tui);
		this.saveValue(taskId, "html", html);
		this.saveValue(taskId, "controllerFunc", controllerFunc);
		this.saveValue(taskId, "instanceNo", instanceNo);
		this.saveValue(taskId, "width", tasklet.width);
		this.saveValue(taskId, "height", tasklet.height);
	}
	
	this.stringify = function(o){
		if(o == null){
			return null;
		}else{
			return Sapl.dynamicToString(o);
		}
	}
		
	this.loadTasklet = function(taskId, o){
		o.st = this.loadValue(taskId, "st");
		o.events = JSON.parse(this.loadValue(taskId, "events"));
		o.resultFunc = this.loadValue(taskId, "resultFunc");
		o.updateFunc = this.loadValue(taskId, "updateFunc");
		o.html = this.loadValue(taskId, "html");
		o.tui = JSON.parse(this.loadValue(taskId, "tui"));
		o.controllerFunc = this.loadValue(taskId, "controllerFunc");
		o.instanceNo = this.loadValue(taskId, "instanceNo");
		o.width = this.loadValue(taskId, "width") | 0;
		o.height = this.loadValue(taskId, "height") | 0;
	}
	
	this.updateTasklet = function(tasklet, html, tuistr){
	
		var taskId = tasklet.taskId;	
		// evaluate it completely to avoid serialize JS objects as much as possible
		var st = DB.stringify(Sapl.heval(tasklet.st)); 
		
		this.saveValue(taskId, "st", st);
		this.saveValue(taskId, "tui", tuistr);
		this.saveValue(taskId, "html", html);	
	}
	
	this.removeTasklet = function(taskId){
		this.removeValue(taskId, "st");
		this.removeValue(taskId, "events");
		this.removeValue(taskId, "resultFunc");
		this.removeValue(taskId, "updateFunc");
		this.removeValue(taskId, "tui");
		this.removeValue(taskId, "html");
		this.removeValue(taskId, "controllerFunc");
		this.removeValue(taskId, "instanceNo");
		this.removeValue(taskId, "width");
		this.removeValue(taskId, "height");			
	}
}

