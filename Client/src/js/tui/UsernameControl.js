Ext.ns("itasks.tui");

itasks.tui.UsernameControl = Ext.extend(Ext.form.ComboBox,{
	store: new Ext.data.JsonStore({
		url: "/dummy",
		root: "users",
		totalProperty: "total",
		fields: ["user"]
	}),
	valueField: "user",
	displayField: "user",
	tpl: new Ext.XTemplate('<tpl for="."><div class="x-combo-list-item">{user:htmlEncode}</div></tpl>'),
	triggerAction: "all",
	listeners: {
		"beforequery": function(e) {
			e.combo.store.proxy.conn.url = itasks.config.serverUrl + "/data/users";
			e.combo.store.baseParams["_session"] = itasks.app.session;
			delete e.combo.lastQuery;
		}
	},
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'span', html: this.value};
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
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
			this.update(value);
		}else{
			itasks.tui.UsernameControl.superclass.setValue.call(this,value);
		}
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		(function() {
			if(msg == "") this.clearInvalid();
			else this.markInvalid(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	}
});

Ext.reg("itasks.tui.Username",itasks.tui.UsernameControl);