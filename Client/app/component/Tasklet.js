Ext.define('itasks.component.Tasklet', {
	extend: 'Ext.Component',
	alias: 'widget.itasks_tasklet',
	
	taskId:  null,
	st: null, // current task state
	
	// script fields
	defState: null,
	script: null,
	events: [],

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
		
		if(this.script != null){
			evalScript(this.script);
		}

		eval("var evalSt = eval(" + this.defState + ");");
		this.st = evalSt;
		controller.tasklets[this.taskId] = this;			
		
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
