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
	store: {
		url: "/services/json/users/names",
		reader: itasks.tui.UsernameReader
	},
	valueField: "username",
	displayField: "username",
	tpl: new Ext.XTemplate('<tpl for="."><div class="x-combo-list-item">{username:htmlEncode}</div></tpl>'),
	triggerAction: "all",
	listeners: {
		"beforequery": function(e) {
			e.combo.store.baseParams["session"] = itasks.app.session;
			delete e.combo.lastQuery;
		}
	},
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: "span", html: this.value};
		}
		
		this.msgTarget = "side";
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.UsernameControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(ct,position){		
		itasks.tui.TimeControl.superclass.afterRender.call(this,ct,position);

		if(this.staticDisplay){
			this.el.next().remove();		
		}	
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},	
	
	setValue: function(value){
		if(this.staticDisplay){
			if(this.el) this.el.dom.innerHTML = value;
		}else{
			itasks.tui.UsernameControl.superclass.setValue.call(this,value);
		}
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		(function() {
			if(msg == "") itasks.tui.common.clearError(this);
			else itasks.tui.common.markError(this,msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	},
	
	getPreferredWidth : function(){
		return 150;
	},
	
	setPreferredWidth : function(width){
		this.setWidth(width);
	}
});

Ext.reg("itasks.tui.Username",itasks.tui.UsernameControl);