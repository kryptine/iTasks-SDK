Ext.define('itwc.component.edit.Editlet',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_edit_editlet',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	
	initComponent: function() {
		var me = this,
			tmp;	

        me.initSize();

        me.htmlId = "editlet-" + me.taskId + "-" + me.editorId;
		itwc.global.controller.editlets[me.htmlId] = me;
		
		if(me.script != null && me.script != "" && !sapldebug){
			evalScript(me.script);
		}
		if(me.defVal != null) {
			eval("tmp = " + me.defVal + ";");
			me.value = Sapl.feval([tmp,[0]]); // the actual argument doesnt matter
			delete this.defVal;
		}
		if(me.appDiff != null) {
			eval("tmp = " + me.appDiff + ";");
			me.appDiff = tmp;	
		}
		if(me.genDiff != null) {
			eval("tmp = " + me.genDiff + ";");
			me.genDiff = tmp;	
		}
		if(me.updateUI != null) {
			eval("tmp = " + me.updateUI + ";");
			me.updateUI = tmp;	
		}		
		if(me.initDiff != null) {
			eval("tmp = " + me.initDiff+ ";");
			me.initDiff = tmp;	
		}		
        if(me.initDiff != null) {
			me.value = Sapl.feval([me.appDiff,[me.initDiff,me.value]]);			
		}

      	me.callParent(arguments);
	},
	afterRender: function() {
		var me = this,
			numEvents = me.events.length,
			el, elName, eventName, expr, i;
	
        if(me.initDiff != null) {
		    me.fireUpdateEvent(me.initDiff);
        } else {
		    me.fireUpdateEvent(__Data_Maybe_Nothing);
        }
	
		for (i = 0; i < numEvents; i++){
			
			elName = me.events[i][0];
			eventName = me.events[i][1];
			expr = eval(me.events[i][2]);
						
			el = Ext.get(elName);
			el.on(eventName, me.eventHandler(true,expr));
		}
		this.callParent(arguments);
	},
	fireUpdateEvent: function (mbDiff) {
		var me = this;
		(me.eventHandler(false,me.updateUI))(mbDiff);		
	},
	// Creating a closure
	eventHandler: function(dowrap,expr){
		var me = this;

		var h = function(event){			

           	if(event) event = event.browserEvent || event;
			if(dowrap) event = ___wrapJS(event);
			var ys = Sapl.feval([expr,[me.htmlId,event,me.value,"JSWorld"]]);
	
			//Strict evaluation of all the fields in the result tuple
			Sapl.feval(ys[2]);
			Sapl.feval(ys[3]);
		
			//Determine diff before overwriting me.value (using superstrict evaluation)
			var diff = me.jsFromSaplJSONNode(Sapl.heval([me.genDiff,[me.value,ys[2]]]));
			
			me.value = ys[2];
			
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
	applyDiff: function (diff) {
		var me = this, tmp;

        eval("tmp = "+diff+";");
		me.value = Sapl.feval([me.appDiff,[tmp,me.value]]);
        me.fireUpdateEvent(tmp);
    },
	//Util functions for exchanging between the values of the clean type Text.JSONNode in
	//the format used in the Sapl interpreter and 'raw' javascript objects
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
