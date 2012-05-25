Ext.define('itasks.component.Tasklet', {
	extend: 'Ext.container.Container',
	alias: 'widget.itasks_tasklet',
	
	taskId:  null,
	st: null, // current task state
	lastResult: null,
	
	// script fields
	defState: null,
	script: null,
	events: [],
	resultFunc: null,
	tui: null,	// default TUI
	controllerFunc: null,
	instanceNo: null,
	
	// indicates whether the defualt TUI is already included or not
	tuiIncluded: false,
	
	// Creating a closure
	eventHandler: function(expr){
		
		var h = function(event){
			eval("var tmp = eval(" + expr + ");");
			tmp[1].push(event.browserEvent); // Stupid ExtJS wraps the original event
			Sapl.feval(tmp);
		};
		
		return h;
	},
	
	onRender: function() {
		
		if(this.script != null && this.script != "" && !sapldebug){
			evalScript(this.script);
		}

		eval("var evalSt = eval(" + this.defState + ");");
		this.st = evalSt;
		controller.tasklets[this.taskId] = this;			

		if(this.resultFunc != null){
			eval("var tmp = eval(" + this.resultFunc + ");");
			this.resultFunc = tmp;
			this.lastResult = Sapl.toJS(Sapl.feval([this.resultFunc,[this.st]]));
		}
		
		if(this.controllerFunc != null){
			controller.taskletControllers[this.instanceNo] = 
					{taskletId: this.taskId, controllerFunc: this.controllerFunc}
		}		
		
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
