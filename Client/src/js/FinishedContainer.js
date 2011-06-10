Ext.ns('itasks');

itasks.FinishedContainer = Ext.extend(Ext.Panel,{
	width: 700,
	height: 150,
	style: {margin: "10px"},
	afterRender: function() {
		this.getEl().fadeOut(
			{ scope: this
			, duration: .75
			, useDisplay: true
			, callback: function() {this.destroyCmp.destroy();}
			}
		);
		itasks.FinishedContainer.superclass.afterRender.call(this,arguments);
	}
});

Ext.reg('itasks.finished',itasks.FinishedContainer);