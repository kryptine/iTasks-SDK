Ext.ns("itasks.form");

itasks.form.StaticField = Ext.extend(Ext.form.Field, {

	format: Ext.util.Format.htmlEncode,
	
	initComponent: function() {
		Ext.apply(this,{
			defaultAutoCreate: {tag: "div"},
			style: "padding: 3px 0px 3px 0px"
		});
		itasks.form.StaticField.superclass.initComponent.apply(this,arguments);
	},
	onRender: function(ct, position) {
		itasks.form.StaticField.superclass.onRender.apply(this,arguments);		
		
		if(!this.el) {
			this.el = ct.createChild(this.getAutoCreate(), position);
		}
		this.setValue(this.value);
	},
	setValue: function(value) {
		this.value = value;
		if(this.rendered) {
			this.el.update(this.format(this.value));
		}
	}
});

itasks.form.InlineField = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			layout: "card",
			width: 200,
			height: 28,
			style: "margin: 0px",
			border: false,
			value: this.field.value,
			activeItem: 0,
			isFormField: true,
			items: [{
				layout: "column",
				border: false,
				items: [{
					xtype: "panel",
					style: "padding: 3px 0px 5px 0px;",
					border: false,
					columnWidth: 1,
					html: (this.format == undefined) ? this.value : this.format(this.value, this.field)
				},{
					xtype: "toolbar",
					border: false,
					width: 28,
					style: "padding: 0px 0px 0px 2px; background: none; border: 0",
					items: [{
						iconCls: "icon-edit",
						cls: "x-btn-icon",
						handler: this.startEdit,
						scope: this
					}]
				}]				
			},{
				layout: "column",
				border: false,
				items: [{
					xtype: "panel",
					border: false,
					layout: "fit",
					columnWidth: 1,
					items: [Ext.apply(this.field,{value: this.value})]
				},{
					xtype: "toolbar",
					border: false,
					width: 28,
					style: "padding: 0px 0px 0px 2px; background: none; border: 0",
					items: [{
						iconCls: "icon-accept",
						cls: "x-btn-icon",
						handler: this.stopEdit,
						scope: this
					}]
				}]		
			}]
		});
		itasks.form.InlineField.superclass.initComponent.apply(this,arguments);
		
		this.addEvents("startedit","stopedit");
	},
	startEdit: function () {
		//Switch to edit card
		this.layout.setActiveItem(1);
		this.doLayout();
		//Fire startedit event
		this.fireEvent("startedit");
	
	},
	stopEdit: function() {
		
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var oldValue = this.value;
		var newValue = field.getValue();
		
		this.setValue(newValue);
	
		//Switch to label card
		this.layout.setActiveItem(0);
		this.doLayout();
		//Fire stopedit event
		this.fireEvent("stopedit",oldValue,newValue);
		//Fire change event
		if(oldValue != newValue)
			this.fireEvent("change",oldValue,newValue);
	},
	getValue: function() {
		return this.value;
	},
	setValue: function(value) {
		
		this.value = value;
		
		var panel = this.getComponent(0).getComponent(0);
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var label = this.format == undefined ? this.value : this.format(this.value, field)
		
		field.setValue(value);
		
		if(panel.rendered)
			panel.getEl().update(label);
		else
			panel.html = label;
	}
});

itasks.form.UserField = Ext.extend(Ext.form.ComboBox,{
	store: new Ext.data.JsonStore({
			root: "users",
			totalProperty: "total",
			fields: ["user","label"],
			//url: itasks.config.serverUrl + "/data/users" //FIX
			url: "/handlers/data/users"
	}),
	valueField: "user",
	displayField: "user",
	tpl: new Ext.XTemplate('<tpl for="."><div class="x-combo-list-item">{user:htmlEncode}</div></tpl>'),
	triggerAction: "all",
	editable: false,
	forceSelection: true,
	initComponent: function () {
		itasks.form.UserField.superclass.initComponent.apply(this,arguments);
		this.addListener("beforequery",function(e) {e.combo.store.baseParams["_session"] = itasks.app.session;});
	}
});

Ext.reg("staticfield", itasks.form.StaticField);
Ext.reg("inlinefield", itasks.form.InlineField);
Ext.reg("itasks.userfield", itasks.form.UserField);

//Global event firing. This may be used by plugins like
//Java applets, Flash or Silverlight components
fireTaskEvent = function(taskid, field, value) {
	
	var ct = Ext.getCmp("taskform-" + taskid);
	if(!ct)
		return;
	
	var wp = ct.findParentByType("itasks.work");
	if(!wp)
		return;
	
	var updates = {}
	updates[field] = value;
	
	wp.sendTaskUpdates(taskid, updates);	
}
