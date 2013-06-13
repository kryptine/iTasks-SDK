Ext.define('itwc.component.edit.Editlet',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_edit_editlet',
	
	width: 600,//'flex',
	minWidth: 400,
	height: 'flex',
	minHeight: 300,
	
	initComponent: function() {
		var me = this,
			tmp;
			
		if(me.script != null && me.script != ""){
			evalScript(me.script);
		}
		if(me.initValue != null) {
			eval("tmp = eval(" + me.initValue + ");");
			me.initValue = tmp;
			me.value = Sapl.feval([me.initValue,[me.jsToSaplJSONNode(me.value)]]);
		}
		if(me.appDiff != null) {
			eval("tmp = eval(" + me.appDiff + ");");
			me.appDiff = tmp;	
		}	
		me.callParent(arguments);
	},
	afterRender: function() {
		var me = this,
			numEvents = me.events.length,
			el, elName, eventName, expr, i;
			
		for (i = 0; i < numEvents; i++){
			
			elName = me.events[i][0];
			eventName = me.events[i][1];
			expr = me.events[i][2];
						
			if(elName == "editlet"){
				if(eventName == "init") {
					(me.eventHandler(expr))(me);
				}
			} else {
				el = Ext.get(elName);
				el.on(eventName, me.eventHandler(expr));
			}
		}
		this.callParent(arguments);
	},
	fireEditletEvent: function (name) {
		var me = this,
			numEvents = me.events.length,
			el, elName, eventName, expr, i;
			
		for (i = 0; i < numEvents; i++){
			elName = me.events[i][0];
				
			if(elName == "editlet"){
				eventName = me.events[i][1];
				if(eventName == name){
					expr = me.events[i][2];
					(me.eventHandler(expr))(me);
					return;
				}
			}
		}	
	},
	// Creating a closure
	eventHandler: function(expr){
		var me = this;
		
		var h = function(event){			
			eval("fun = eval(" + expr + ");");
		
			var ys = Sapl.feval([fun,[me.value,me.editorId,event.browserEvent,document]]);
	
			//Strict evaluation of both the fields in the result tuple
			Sapl.feval(ys[2]);
			Sapl.feval(ys[3]);
	
			me.value = ys[3];
		};
		return h;
	},
	applyValue: function (val) {
		this.value = val;
	},
	applyDiff: function (diff) {
		var me = this;
		
		me.value = Sapl.feval([me.appDiff,[me.jsToSaplJSONNode(diff),me.value]]);
		me.fireEditletEvent("update");
	},
	//Util functions for exchanging between the values of the clean type Text.JSONNode in
	//the format used in the Sapl interpreter and 'raw' javascript objects
	jsToSaplJSONNode: function (obj) {
		var me = this,
			args, i, k;
		
		if(obj === null) {
			return [0,"JSONNull"];
		}
		switch(typeof(obj)) {
			case "boolean":
				return [1,"JSONBool",obj];
			case "number":
				if(isInteger(obj)) {
					return [2,"JSONInt",obj];
				} else {
					return [3,"JSONReal",obj];
				}
			case "string":
				return [4,"JSONString",obj]
			case "object": //Null, array or object
				if(isArray(obj)) {
					//Don't use Sapl.toList to prevent going through the array twice
					args = [1,"_predefined._Nil"];
					for(i = obj.length - 1; i >= 0; i--) {
						args = [0,"_predefined._Cons",me.jsToSaplJSONNode(obj[i]),args];
					}
					return [5,"JSONArray",args];
				} else {
					args = [];
					i = 0;
					for(k in obj) {
						args[i++] = [k,me.jsToSaplJSONNode(obj[k])];
					}
					return [6,"JSONObject",Sapl.toList(args)];
				}
		}
		return [8,'JSONError'];
	},	
	jsFromSaplJSONNode: function (sapl) {
		return sapl;//TODO
	}
})
