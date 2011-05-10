Ext.ns('itasks');

itasks.FinishedContainer = Ext.extend(Ext.Panel,{
	width: 700,
	height: 150,
	margins: {left: 10, top: 10, right: 10, bottom: 10},
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