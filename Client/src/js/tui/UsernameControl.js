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
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.UsernameControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Username",itasks.tui.UsernameControl);