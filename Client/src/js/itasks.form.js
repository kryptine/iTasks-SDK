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

Ext.reg("staticfield", itasks.form.StaticField);