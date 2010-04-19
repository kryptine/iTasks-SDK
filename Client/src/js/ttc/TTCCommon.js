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
				comp.on("select",changeTaskEvent);
				break;
			case "button":
			case "menuitem":
				if(comp.name)
					comp.on("click",clickTaskEvent);
				break;
			case "itasks.tui.FormattedText":
				comp.on("sync",changeTaskEvent);
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
		
		if(link.pathname.indexOf('/') != 0){
			link.pathname = itasks.config.serverUrl+'/'+link.pathname;
		}else{
			link.pathname = itasks.config.serverUrl+link.pathname;
		}
		link.href = Ext.urlAppend(link.href,'_session='+itasks.app.session);
		link.name = "";
		
		for(var y=0; y < plinks.length; y++){				
			if(plinks[y].id == link.id){
				var plink = plinks[y];		

				plink.href="javascript:itasks.preview('"+link.href.replace( 'download','preview')+"')";
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
			this.html = this.description;
		}

		itasks.ttc.common.DescriptionPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.common.description', itasks.ttc.common.DescriptionPanel);