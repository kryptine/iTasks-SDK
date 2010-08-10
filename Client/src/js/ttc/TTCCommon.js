Ext.ns('itasks.ttc.common');

itasks.ttc.common.attachTaskHandlers = function(comp,taskId){

	// Scary hack! Don't look below!
	new Ext.util.DelayedTask().delay(100,itasks.ttc.common.attachDocumentLinkInformation,comp);
	// End of scary hack
		
	var changeTaskEvent = function () {
			
		var ct = this.findParentByType(itasks.ttc.FormContainer);
		if (!ct)
			ct = this.findParentByType(itasks.ttc.InstructionContainer);
		if(!ct)
			ct = this.findParentByType(itasks.ttc.MessageContainer);
		if (!ct)
			// if element is not inside form/instr/message it's a group's toolbar
			ct = this.findParentByType(itasks.ttc.GroupContainer);
		if(!ct) return;
		
		//Helper function to get the value of a checkbox group
		function checkboxValues(boxes) {
			var values = [];
			var num = boxes.length;
			for(var i = 0; i < num; i++) {
				values[values.length] = boxes[i].value;
			}
			return Ext.encode(values);
		}
		
		var value;
		switch(this.xtype) {
			case "itasks.tui.Date": value = this.getRawValue(); break;
			case "radiogroup": value = this.getValue().value; break;
			case "checkboxgroup": value = checkboxValues(arguments[1]); break;
			default: value = this.getValue();
		}
		ct.addUpdate(this.name, value);
		ct.sendUpdates(true);
	};
	var clickTaskEvent = function () {
		
		if(this.clickCB) this.clickCB(this);
		
		var ct = this.findParentByType(itasks.ttc.FormContainer);
		if (!ct)
			ct = this.findParentByType(itasks.ttc.InstructionContainer);
		if(!ct)
			ct = this.findParentByType(itasks.ttc.MessageContainer);
		// if element is not inside form/instr/message it's a group's toolbar
		if (!ct)
			ct = this.findParentByType(itasks.ttc.GroupContainer);
		if(!ct) return;
			
		var taskId = (comp.taskId)? comp.taskId : ct.taskId
			
		ct.addUpdate(this.name, this.value);
		ct.sendUpdates();
	};	
	var choiceTaskEvent  = function(){
		var ct = this.findParentByType(itasks.ttc.FormContainer);
		if (!ct)
			ct = this.findParentByType(itasks.ttc.InstructionContainer);
		if(!ct)
			ct = this.findParentByType(itasks.ttc.MessageContainer);
		// if element is not inside form/instr/message it's a group's toolbar
		if (!ct)
			ct = this.findParentByType(itasks.ttc.GroupContainer);
		if(!ct) return;
		
		
		ct.addUpdate(this.name, Ext.encode(comp.getValue()));
		ct.sendUpdates(true);
	}
	
	switch(comp.getXType()) {
			case "textfield":
			case "itasks.tui.String":
			case "itasks.tui.Char":
			case "itasks.tui.Int":
			case "itasks.tui.Real":
			case "itasks.tui.Note":
			case "itasks.tui.Date":
			case "itasks.tui.Time":
			case "itasks.tui.Username":
			case "itasks.tui.Currency":
			case "itasks.tui.Password":
			case "textarea":
			case "numberfield":
			case "datefield":
			case "timefield":
			case "radiogroup":
				comp.on("change",changeTaskEvent);
				break;
			case "checkbox":
			case "itasks.tui.Bool":
				comp.on("check",changeTaskEvent);
				break;
			case "checkboxgroup":
				comp.on("change",changeTaskEvent);
				break;
			case "combo":
			case "itasks.userfield":
			case "itasks.tui.ColorChooser":
				comp.on("select",changeTaskEvent);
				break;
			case "button":
			case "itasks.tui.FormButton":
			case "menuitem":
				if(comp.name)
					comp.on("click",clickTaskEvent);
				break;
			case "itasks.tui.FormattedText":
			case "itasks.tui.SourceCode":
				comp.on("update",changeTaskEvent);
				break;
			case "itasks.tui.Choice":
				comp.on("change",choiceTaskEvent);
				break;
	}
	if(comp.buttons) {
		var num = comp.buttons.length;
		for(var i = 0; i < num; i++) {
			comp.buttons[i].on("click",clickTaskEvent);
		}
	}
	//attach recursively
	if(comp.items && comp.items.each)
		comp.items.each(itasks.ttc.common.attachTaskHandlers, comp, taskId);
	if(comp.menu)
		itasks.ttc.common.attachTaskHandlers(comp.menu,taskId);
	if(comp.topToolbar)
		itasks.ttc.common.attachTaskHandlers(comp.topToolbar,taskId);
};

itasks.ttc.common.attachDocumentLinkInformation = function() {
	
	var links   = Ext.query("a[name=x-form-document-link]");
	var plinks = Ext.query("a[name=x-form-document-preview-link]");
	
	for(var x=0; x < links.length; x++){
		var link = links[x];
		
		link.href = Ext.urlAppend(link.href,'session='+itasks.app.session);
		link.name = "";
		
		for(var y=0; y < plinks.length; y++){				
			if(plinks[y].id == link.id){
				var plink = plinks[y];		

				plink.href="javascript:itasks.util.preview('"+link.href.replace( 'download','preview')+"')";
				plink.name = "";
			}
		}
	}
};

itasks.ttc.common.DescriptionPanel = Ext.extend(Ext.Panel,{
	initComponent : function(){
		
		Ext.apply(this,{
			cls: this.cls + ' task-description',
			unstyled: true
		});
		
		if(Ext.isDefined(this.headerButton)) {
			this.items = [{html: this.description, unstyled: true, columnWidth: 1}, this.headerButton];
			this.layout = 'column';
		} else {
			this.items = [{html: this.description, unstyled: true}];
		}

		itasks.ttc.common.DescriptionPanel.superclass.initComponent.apply(this,arguments);
	}
});

itasks.ttc.common.setFullscreen = function(ct){
	ct.fullscreen = true;
};

itasks.ttc.common.setupHotkeys = function(tb, parent) {
	// collect all hotkeys
	var hotkeys = new Ext.util.MixedCollection();
	
	var collectHotkeys = function(item) {
		var hk = item.hotkey;
		if(hk) {
			hotkeys.add({hotkey: hk, menuitem: item});
			// append hotkey-string to item name
			var str = hk.key;
			if (hk.shift)	str = "Shift+" + str;
			if (hk.alt)		str = "Alt+" + str;
			if (hk.ctrl)	str = "Ctrl+" + str;
			item.setText(item.text + "&nbsp;&nbsp;&nbsp;&nbsp;(" + str + ")");
		} else if (item.items || item.menu) {
			var children =  item.items || item.menu.items;
			for(var i = 0; i < children.length; i++) {
				collectHotkeys(children.get(i));
			}
		}
	}
	
	collectHotkeys(tb);
	
	// disabled old hotkeys
	if (parent.keyMap)
		parent.keyMap.disable();

	if (hotkeys.getCount() == 0)
		return;
	
	// build up config array for KeyMap
	var conf = new Array();
	hotkeys.each(function(hk) {
		var h = hk.hotkey;
		conf[conf.length] = {
			key: h.key,
			ctrl: h.ctrl,
			alt: h.alt,
			shift: h.shift,
			stopEvent: true,
			handler: function() {
				var item = hk.menuitem;
				// only fire hotkey-event if item is not disabled
				if (!item.disabled) {
					parent.addUpdate(item.name, item.value);
					parent.sendUpdates();
				}
			}
		};
	});

	parent.keyMap = new Ext.KeyMap(parent.getEl(), conf);
};

Ext.reg('itasks.ttc.common.description', itasks.ttc.common.DescriptionPanel);