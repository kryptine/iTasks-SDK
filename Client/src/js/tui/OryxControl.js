Ext.ns('itasks.tui');

itasks.tui.OryxControl = Ext.extend(Ext.Panel,{
	initComponent : function(){
		Ext.apply(this,
			{ url: "/handlers/work/tab"
			, border: false
            , layout: 'fit'
            , height: 240
            , html: 'Loading...'
			});

		itasks.tui.OryxControl.superclass.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');
        this.inUpdate = false;

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
        json = Ext.decode(value);
        if (! itasks.util.approxEquals(this.facade.getJSON(), json, 1E-6)) {
            this.inUpdate = true; //temporary ignore onChange events
            this.clearEditor();
            this.facade.importJSON(json);
            this.inUpdate = false;
        }
	},

    clearEditor : function(){
        var clearChildren = function(node) {
          while (node.hasChildNodes())
              node.removeChild(node.firstChild);
        };

        var canvas = this.facade.getCanvas();
        canvas.children.clear();
        //Remove nodes
        canvas.nodes.clear();
        clearChildren (canvas.node.childNodes[0].childNodes[1]);
        //Remove edges
        canvas.edges.clear();
        clearChildren (canvas.node.childNodes[0].childNodes[2]);
    },

	onChange : function(){
        if (! this.inUpdate)
		    this.fireEvent('tuichange',this.name,Ext.encode(this.facade.getJSON()));
	},
	
	afterRender : function(){
	    itasks.tui.OryxControl.superclass.afterRender.call(this);

		switch(itasks.app.oryxState) {
			case 'loaded':
				this.buildEditor();
				break;
			case 'unloaded':
				itasks.app.oryxState = 'loading';

            	var script = document.createElement("script");
            	script.setAttribute('type', 'text/javascript');
            	script.setAttribute('src', 'oryx-all.js');
            	Ext.getHead().appendChild(script);
			case 'loading':
				itasks.app.waitingForOryx.addAll([this.buildEditor.createDelegate(this)]);
		}
	},

    buildEditor: function() {
        var url = this.stencilsetURL[0] == '/' 
                  ? this.stencilsetURL
                  : ORYX.CONFIG.ROOT_PATH + 'stencilsets/' + this.stencilsetURL;

		this.facade = new ORYX.Editor({
            parentContainer: this,
            stencilset: {
                url: url
            }
        });

        var oryxControl = this;
        this.facade.registerOnEvent(ORYX.CONFIG.EVENT_AFTER_EXECUTE_COMMANDS, 
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

window.onOryxResourcesLoaded = function() {
	itasks.app.waitingForOryx.each(function(build){build();});
	itasks.app.oryxState = 'loaded';
};

Ext.reg('itasks.tui.Oryx', itasks.tui.OryxControl);
