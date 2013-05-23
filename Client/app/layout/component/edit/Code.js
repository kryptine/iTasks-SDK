Ext.define('itwc.layout.component.edit.Code',{
	extend: 'Ext.layout.component.Auto',
	alias: 'layout.itwc_edit_code',
	type: 'edit_code',

	beginLayout: function(ownerContext) {
		this.callParent(arguments);
		
		ownerContext.editorContext = ownerContext.getEl('editorEl');
	},
	publishInnerHeight: function (ownerContext, height) {
		var me = this,
			innerHeight = height - ownerContext.getFrameInfo().height;	
		if (Ext.isNumber(innerHeight)) {
			ownerContext.editorContext.setHeight(innerHeight);
		} else {
			me.done = false;
		}
    },
	publishInnerWidth: function (ownerContext, width) {
		var me = this;

		if (Ext.isNumber(width)) {
			ownerContext.editorContext.setWidth(width);
 		} else {
			me.done = false;
		}
	}
});
