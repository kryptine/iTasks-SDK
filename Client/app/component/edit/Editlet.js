Ext.define('itwc.component.edit.Editlet',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_edit_editlet',
	mixins: ['itwc.component.edit.Editable'],
	
	width: 600,//'flex',
	minWidth: 400,
	height: 'flex',
	minHeight: 300,
	
	initComponent: function() {
		var me = this,
			tmp;	

        me.htmlId = "editlet-" + me.taskId + "-" + me.editorId;
		me.state = [0,"Data.Maybe.Nothing"];
		
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
		if(me.genDiff != null) {
			eval("tmp = eval(" + me.genDiff + ");");
			me.genDiff = tmp;	
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
				} else {
					el = Ext.get(me.htmlId); //FIXME
                    if(el) {
					    el.on(eventName, me.eventHandler(expr));
                    }
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
			eval("var fun = eval(" + expr + ");");
		
			var ys = Sapl.feval([fun,[me.htmlId,event.browserEvent,me.value,me.state,"JSWorld"]]);
	
			//Strict evaluation of all the fields in the result tuple
			Sapl.feval(ys[2]);
			Sapl.feval(ys[3]);
			Sapl.feval(ys[4]);
			
			//Determine diff before overwriting me.value (using superstrict evaluation)
			var diff = me.jsFromSaplJSONNode(Sapl.heval([me.genDiff,[me.value,ys[2]]]));
			
			me.value = ys[2];
			me.state = ys[3];			
			
			//Synchronize
			if(diff !== null) {
				var val = me.getEditorValue();
				me.lastEditNo = itwc.global.controller.sendEditEvent(me.taskId,me.editorId,diff);
			}
		};
		return h;
	},
    getValue: function () {
        return this.value;
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
			return [0,"Text.JSON.JSONNull"];
		}
		switch(typeof(obj)) {
			case "boolean":
				return [1,"Text.JSON.JSONBool",obj];
			case "number":
				if(isInteger(obj)) {
					return [2,"Text.JSON.JSONInt",obj];
				} else {
					return [3,"Text.JSON.JSONReal",obj];
				}
			case "string":
				return [4,"Text.JSON.JSONString",obj]
			case "object": //Null, array or object
				if(isArray(obj)) {
					//Don't use Sapl.toList to prevent going through the array twice
					args = [1,"_predefined._Nil"];
					for(i = obj.length - 1; i >= 0; i--) {
						args = [0,"_predefined._Cons",me.jsToSaplJSONNode(obj[i]),args];
					}
					return [5,"Text.JSON.JSONArray",args];
				} else {
                    args = [1,"_predefined._Nil"];
                    for(k in obj) {
                        args = [0,"_predefined._Cons",Sapl.toTuple([k,me.jsToSaplJSONNode(obj[k])]),args];
                    }
                    return [6,"Text.JSON.JSONObject",args];
				}
		}
		return [8,'JSONError'];
	},	
	jsFromSaplJSONNode: function (sapl) {
		switch(sapl[0]) {
			case 0:	return null;
			case 1: return sapl[2];
			case 2: return sapl[2];
			case 3: return sapl[2];
			case 4: return sapl[2];
			case 5: return this.jsFromList(sapl[2]);
			case 6:
				return this.jsFromFieldList({},sapl[2]);			
		}
	},
	jsFromList: function(sapl) {
		if(sapl[0] == 0) {
			return ([this.jsFromSaplJSONNode(sapl[2])]).concat(this.jsFromList(sapl[3]));
		} else {
			return [];
		}
	},
	jsFromFieldList: function (fields,sapl) {
		
		if(sapl[0] == 0) {
			fields = this.jsFromField(fields,sapl[2]);
			fields = this.jsFromFieldList(fields,sapl[3]);
		}
		return fields;
	},
	jsFromField: function (fields,sapl) {
		fields[sapl[2]] = this.jsFromSaplJSONNode(sapl[3]);
		return fields;
	}
})
