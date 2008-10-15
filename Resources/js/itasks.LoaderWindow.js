/**
 * Specialized progress bar for iTasks load process
 */
Ext.ns('itasks');

itasks.LoaderWindow = Ext.extend(Ext.Window, {

	progressBar: new Ext.ProgressBar({
		text: 'Initializing...'
	}),

	updateProgress: function(i, msg) {
		this.progressBar.updateProgress(i,msg);
	},
	finish: function () {
		this.getEl().fadeOut({
			callback: function() {
				this.continuation();
			},
			scope: this
		});
	},
	initComponent: function() {
		Ext.apply(this, {
			y: 150,
			width: 350,
			height: 50,
			hidden: true,
			bodyStyle: 'padding: 5px',
			closable: false,
			resizable: false,
			items: this.progressBar
		});

		itasks.LoaderWindow.superclass.initComponent.apply(this, arguments);	
	},
	continuation: function () {
	}
});
