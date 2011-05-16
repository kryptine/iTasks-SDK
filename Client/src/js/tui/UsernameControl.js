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

itasks.tui.UsernameControl = itasks.tui.extendControl(Ext.form.ComboBox,{
	defaultWidth: ['Fixed',100],
	defaultHeight: ['Fixed',25],
	triggerAction:	'all',
	valueField:		'username',
	displayField:	'username',
	store:			{url: itasks.config.serviceUrl + "/json/users/names", reader: itasks.tui.UsernameReader},
	
	initComponent: function() {
		this.listeners = {
			change: {fn: this.onChange, scope: this},
			beforequery: function(e) {
				e.combo.store.baseParams["session"] = itasks.app.session;
				delete e.combo.lastQuery;
			}
		};
		this.tpl = new Ext.XTemplate('<tpl for="."><div class="x-combo-list-item">{username:htmlEncode}</div></tpl>');
		
		itasks.tui.control.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Username",itasks.tui.UsernameControl);