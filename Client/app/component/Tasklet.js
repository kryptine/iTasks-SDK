Ext.define('itasks.component.Tasklet', {
	extend: 'Ext.container.Container',
	alias: 'widget.itasks_tasklet',
	
	taskId:  null,
	
	// script fields
	script: null,
	st: null, // current task state	
	events: [],
	resultFunc: null,
	tui: null,	// default TUI
	controllerFunc: null,
	instanceNo: null,
	
	// indicates whether the defualt TUI is already included or not
	tuiIncluded: false,
	// for detecting whether the result is changed
	lastResult: null,
	
	// Creating a closure
	eventHandler: function(expr){
		
		var h = function(event){
			eval("var tmp = eval(" + expr + ");");
			tmp[1].push(event.browserEvent); // Stupid ExtJS wraps the original event
			Sapl.feval(tmp);
		};
		
		return h;
	},
	
	initComponent: function() {
		
		if(this.script != null && this.script != "" && !sapldebug){
			evalScript(this.script);
		}

		eval("var tmp = eval(" + this.st + ");");
		this.st = tmp;
		controller.tasklets[this.taskId] = this;			

		if(this.resultFunc != null){
			eval("var tmp = eval(" + this.resultFunc + ");");
			this.resultFunc = tmp;
			this.lastResult = Sapl.toJS(Sapl.feval([this.resultFunc,[this.st]]));
		}
		
		if(this.controllerFunc != null){
			eval("var tmp = eval(" + this.controllerFunc + ");");
			this.controllerFunc = tmp;
			controller.taskletControllers[this.instanceNo] = 
					{taskletId: this.taskId, controllerFunc: this.controllerFunc}
		}			
	
		DB.saveTasklet(this);
	
		this.callParent(arguments);
	},	
	
	onRender: function() {
				
		if(this.tui != null && !this.tuiIncluded){
			this.tuiIncluded = true;
			this.tui = this.lookupComponent(this.tui);
			this.add(this.tui);
		}		
				
		this.callParent(arguments);
	},
	
	afterRender: function() {

		for (var i=0; i<this.events.length; ++i){
			var elname = this.events[i][0];
			var eventName = this.events[i][1];
			var expr = this.events[i][2];
						
			if(elname == "tasklet"){
				if(eventName == "init"){
					(this.eventHandler(expr))(this);
				}
			}else{
				var el = Ext.get(elname);
				el.on(eventName, this.eventHandler(expr));
			}
		}
		
		this.callParent(arguments);
	}	
	
});
