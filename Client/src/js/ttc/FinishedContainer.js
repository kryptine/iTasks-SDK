Ext.ns('itasks.ttc');

itasks.ttc.FinishedContainer = Ext.extend(Ext.Panel,{
	width: 700,
	height: 150,
	margins: {left: 10, top: 10, right: 10, bottom: 10},
	afterRender: function() {
		this.getEl().fadeOut(
			{ scope: this
			, duration: itasks.ttc.TTC_FADE_DURATION
			, useDisplay: true
			, callback: function() {this.destroyCmp.destroy();}
			}
		);
		itasks.ttc.TTCBase.superclass.afterRender.call(this,arguments);
	}
});

Ext.reg('itasks.ttc.finished',itasks.ttc.FinishedContainer);