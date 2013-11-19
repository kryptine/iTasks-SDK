Ext.define('itwc.container.Tasklet', {
	extend: 'Ext.container.Container',
	alias: 'widget.itwc_tasklet',
    mixins: ['itwc.Sizeable'],
	
	taskId:  null,
	
	// script fields
	script: null,
	st: null, // current task state	
	events: [],
	resultFunc: null,
	updateFunc: null,
	updateVal: null,
	tui: null,	// default TUI
	controllerFunc: null,
	instanceNo: null,
	
	updater: null,
	
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

	setTaskId: function(newTaskId) {
		this.taskId = newTaskId;
	},
	
	selfUpdate: function(newTasklet) {
		if(this.taskId != newTasklet.taskId)
					this.setTaskId(newTasklet.taskId);
					
		if(this.updateFunc != null && newTasklet.updateVal != null){
			eval("var tmp = eval(" + this.updateFunc + ");");
			this.updateFunc = tmp;
			eval("var tmp = eval(" + newTasklet.updateVal + ");");
			this.updateVal = tmp;
			
			this.st = Sapl.feval([this.updateFunc,[this.updateVal, this.st]]);
			this.fireTaskletEvent("update");
		}					
	},	
	
	initComponent: function() {

        this.initSize();

		if(this.script != null && this.script != "" && !sapldebug){
			evalScript(this.script);
		}		

		eval("var tmp = eval(" + this.st + ");");
		this.st = Sapl.feval(tmp);
		itwc.global.controller.tasklets[this.taskId] = this;

		if(!_iworld){
			_iworld = Sapl.fapp(__iTasks_Framework_Client_RunOnClient_createClientIWorld, [this.instanceNo]);
		}
				
		if(this.resultFunc != null){
			eval("var tmp = eval(" + this.resultFunc + ");");
			this.resultFunc = tmp;
			this.lastResult = Sapl.toJS(Sapl.feval([this.resultFunc,[this.st]]));
		}
		
		if(this.controllerFunc != null){
			var start = new Date().getTime();
		
			eval("var tmp = eval(" + this.controllerFunc + ");");
			this.controllerFunc = tmp;
			itwc.global.controller.taskletControllers[this.instanceNo] = this;
			
			var ret = Sapl.fapp(this.controllerFunc, [this.taskId, this.st, 0, __Data_Maybe_Nothing, __Data_Maybe_Nothing, _iworld]);
			this.st = Sapl.feval(ret[3]);
			_iworld = Sapl.feval(ret[4]);
			
			var ui = Sapl.toJS(Sapl.feval(ret[2]));
			
			this.tui = JSON.parse(ui);
			if (this.tui.xtype=="itwc_viewport") this.tui = this.tui.items[0];
						
			var end = new Date().getTime();			
			console.log(end-start);
		}			
		
		//DB.saveTasklet(this);
		
		this.callParent(arguments);
	},	
	
	onRender: function() {
				
		if(this.tui != null && !this.tuiIncluded){
			this.tuiIncluded = true;
			this.tui = this.lookupComponent(this.tui);
			this.add(this.tui);
			
			var me = this;
			
			me.updater = window.setInterval(function(){
				
				var ret = Sapl.fapp(me.controllerFunc, [me.taskId, me.st, 0, __Data_Maybe_Nothing, __Data_Maybe_Nothing, _iworld]);
				me.st = Sapl.feval(ret[3]);
				_iworld = Sapl.feval(ret[4]);			
			
				var tui = Sapl.toJS(Sapl.feval(ret[2]));
				tui = JSON.parse(tui);
				if (tui.xtype=="itwc_viewport") tui = tui.items[0];			
			
				applytui(me.tui, tui);
			
			},500);
			
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
	},
	
	fireTaskletEvent: function(name){
		for (var i=0; i<this.events.length; ++i){
			var elname = this.events[i][0];
			var eventName = this.events[i][1];
			var expr = this.events[i][2];
						
			if(elname == "tasklet"){
				if(eventName == name){
					(this.eventHandler(expr))(this);
					break;
				}
			}
		}	
	},
	
	onDestroy: function() {

		if(this.updater){
			window.clearInterval(this.updater);
		}
	
		this.fireTaskletEvent("destroy");
		this.callParent(arguments);
	},
	
	afterComponentLayout: function() {
	
		this.fireTaskletEvent("afterlayout");
		this.callParent(arguments);
    }
});
