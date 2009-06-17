Ext.ns("itasks.form");

itasks.form.StaticField = Ext.extend(Ext.form.Field, {

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
			this.el.update(Ext.util.Format.htmlEncode(this.value));
		}
	}
});

itasks.form.InlineField = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			layout: 'card',
			border: false,
			value: this.field.value,
			activeItem: 0,
			isFormField: true,
			
			items: [{
				layout: 'column',
				border: false,
				items: [{
					xtype: 'panel',
					style: 'padding: 3px 0px 5px 0px;',
					border: false,
					columnWidth: 1,
					html: (this.format == undefined) ? this.field.value : this.format(this.field.value, this.field)
				},{
					xtype: 'toolbar',
					border: false,
					width: 28,
					style: 'padding: 0px 0px 0px 2px; background: none; border: 0',
					items: [{
						icon: 'img/icons/pencil.png',
						cls: 'x-btn-icon',
						handler: this.startEdit,
						scope: this
					}]
				}]				
			},{
				layout: 'column',
				border: false,
				items: [{
					xtype: 'panel',
					border: false,
					layout: 'fit',
					columnWidth: 1,
					items: [this.field]
				},{
					xtype: 'toolbar',
					border: false,
					width: 28,
					style: 'padding: 0px 0px 0px 2px; background: none; border: 0',
					items: [{
						icon: 'img/icons/accept.png',
						cls: 'x-btn-icon',
						handler: this.stopEdit,
						scope: this
					}]
				}]		
			}]
		});
		itasks.form.InlineField.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('startedit','stopedit');
	},
	startEdit: function () {
		//Switch to edit card
		this.layout.setActiveItem(1);
		this.doLayout();
		//Fire startedit event
		this.fireEvent('startedit');
	
	},
	stopEdit: function() {
		
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var oldValue = this.value;
		var newValue = field.getValue();
		
		this.value = newValue;
		
		//Update the label
		this.setLabel((this.format == undefined) ? this.value : this.format(this.value, field));
	
		//Switch to label card
		this.layout.setActiveItem(0);
		this.doLayout();
		//Fire stopedit event
		this.fireEvent('stopedit',oldValue,newValue);
	},
	setLabel: function(msg) {
		this.getComponent(0).getComponent(0).getEl().update(msg);
	},
	getValue: function() {
		return this.value;
	},
	setValue: function(value) {
		this.value = value;
		
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		
		field.setValue(value);
		
		this.setLabel((this.format == undefined) ? this.value : this.format(this.value, field));
	}
});

Ext.reg("staticfield", itasks.form.StaticField);
Ext.reg("inlinefield", itasks.form.InlineField);
