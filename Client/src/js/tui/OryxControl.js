Ext.ns('itasks.tui');

itasks.tui.OryxControl = itasks.tui.extendControl(Ext.Panel,{
	initComponent : function(){
		this.defaultWidth = 1000;
		this.defaultHeight = 700;
		Ext.apply(this,
			{ url: "/handlers/work/tab"
			, border: false
            , layout: 'fit'
            , height: 320
            , cls: 'oryxcontrol'
            , html: 'Loading...'
			});

		itasks.tui.control.initComponent.apply(this,arguments);

        this.inUpdate = false;
	},

	setValue : function(json){
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
		    this.fireEvent('tuichange',this.taskId,this.name,this.facade.getJSON());
	},
	
	afterRender : function(){
	    itasks.tui.OryxControl.superclass.afterRender.apply(this,arguments);

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
        
        this.facade.importJSON(this.value);

        var oryxControl = this;
        this.facade.registerOnEvent(ORYX.CONFIG.EVENT_AFTER_EXECUTE_COMMANDS, 
                                    function(){ oryxControl.onChange(); });

		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
    },
	
	setError: function(message){		
        //OryxControl does not support error messages, only hints
    },
	
	setHint: function(message){
        this.facade.handleEvents({
            type: ORYX.Plugins.SyntaxChecker.RESET_ERRORS_EVENT
        });
        if (message == '')
            return;

        json = Ext.decode(message);
        var hints = {};
        json.each(function(hint){
                      hints[hint.resourceId] = (hint.paramIndex != null
                                                ? 'Parameter ' + (hint.paramIndex + 1) + ': ' 
                                                : ''
                                               ) + hint.message;
                  }.bind(this));
        
        this.facade.handleEvents({
            type: ORYX.Plugins.SyntaxChecker.SHOW_ERRORS_EVENT,
            errors: hints
        });
	}
});

window.onOryxResourcesLoaded = function() {
	itasks.app.waitingForOryx.each(function(build){build();});
	itasks.app.oryxState = 'loaded';
};

Ext.reg('itasks.tui.Oryx', itasks.tui.OryxControl);
