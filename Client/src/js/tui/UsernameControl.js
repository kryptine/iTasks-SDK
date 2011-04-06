Ext.ns("itasks.tui");

itasks.tui.UsernameReader = new Ext.data.JsonReader();
itasks.tui.UsernameReader.readRecords = function(o) {
	var UsernameRecord = Ext.data.Record.create(["username"]);	
	var usernames = [];
	for(var i = 0, len = o.users.length; i < len; i++) {
		
		usernames[usernames.length] = new UsernameRecord({username: o.users[i]});
	}
	return {success: true, records: usernames, totalRecords: usernames.length};
}

itasks.tui.UsernameControl = Ext.extend(Ext.form.ComboBox,{
	
	initComponent: function() {
		this.msgTarget = "side";
		this.listeners = {
			change: {fn: this.onChange, scope: this},
			beforequery: function(e) {
				e.combo.store.baseParams["session"] = itasks.app.session;
				delete e.combo.lastQuery;
			}
		};
		this.triggerAction = "all";
		this.valueField = "username";
		this.displayField = "username";
		
		this.store = {
			url: itasks.config.serviceUrl + "/json/users/names",
			reader: itasks.tui.UsernameReader
		};
		this.tpl = new Ext.XTemplate('<tpl for="."><div class="x-combo-list-item">{username:htmlEncode}</div></tpl>');
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		if(this.value == "") delete this.value;
		itasks.tui.UsernameControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange', this.taskId, this.name, this.getValue());	
	},
	afterRender: function(ct,position){		
		itasks.tui.UsernameControl.superclass.afterRender.call(this,ct,position);

		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},	
	setValue: function(value){
		itasks.tui.UsernameControl.superclass.setValue.call(this,value);
		
		if(this.activeError)
			this.setError(this.activeError);
	},
	
	setError: function(msg){
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	}
});

Ext.reg("itasks.tui.Username",itasks.tui.UsernameControl);