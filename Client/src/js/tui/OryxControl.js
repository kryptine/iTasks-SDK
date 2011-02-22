Ext.ns('itasks.tui');

itasks.tui.OryxControl = Ext.extend(Ext.Panel,{
	initComponent : function(){
		Ext.apply(this,
			{ url: "/handlers/work/tab"
			, border: false
			, autoHeight: false
            , layout: 'fit'
			});			
		
		itasks.tui.OryxControl.superclass.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');

		var oryxControl = this;
		this.bubble(function(ct){ 
			if (this.xtype == 'itasks.ttc.interactive') {
				this.on ('resize', 
					function(panel, w, h) {
						oryxControl.setHeight(this.getHeight() - 90);
					});
			}
		});
	},

	setValue : function(value){
        if (console != null)
            console.log('OryxControl.setValue: ' + value);
//        this.facade.loadSerialized(Ext.decode(value));
	},

	onChange : function(){
        if (console != null)
            console.log('OryxControl.onChange(' + Ext.encode(this.facade.getJSON()) + ')');
		this.fireEvent('tuichange',this.name,Ext.encode(this.facade.getJSON()));
	},
	
	afterRender : function(){
	    itasks.tui.OryxControl.superclass.afterRender.call(this);

        this.facade = new ORYX.Editor({
            parentContainer: this,
            stencilset: {
                url: ORYX.CONFIG.ROOT_PATH + this.stencilsetURL
            }
        });

        var oryxControl = this;
        this.facade.registerOnEvent(ORYX.CONFIG.EVENT_EXECUTE_COMMANDS, 
                                    function(){ oryxControl.onChange(); });

		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	setError: function(msg){		
        if (console != null)
            console.log('OryxControl.setError: ' + msg);
	},
	
	setHint: function(msg){
        if (console != null)
            console.log('OryxControl.setHint: ' + msg);
	}
});

//override onOryxResourcesLoaded in lib/oryx/scripts/Plugins/file.js
window.onOryxResourcesLoaded = function() { }
{
    //TODO: Use as callback for on-request loading of ORYX
};

Ext.reg('itasks.tui.Oryx', itasks.tui.OryxControl);
