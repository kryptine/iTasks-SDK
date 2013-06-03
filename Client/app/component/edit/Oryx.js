var oryxState = 'unloaded';
var waitingForOryx = []; // Track components that wait for the api to load

// Main impl
Ext.define('itwc.component.edit.Oryx', {
  extend: 'Ext.panel.Panel',
  alias: 'widget.itwc_edit_oryx',
  mixins: ['itwc.component.edit.Editable'],
  width: 'flex',
  defaultWidth: 600,
  height: 'flex',
  defaultHeight: 400,

  initComponent : function() {
    Ext.apply(this,
      { url: "/handlers/work/tab",
        border: false,
        layout: 'fit',
        cls: 'oryxcontrol',
        html: 'Loading...'
      });

    this.callParent(arguments);// TODO Or callSuper?

    this.inUpdate = false;
  },

  //Function to determine equality of values,
  //modulo diff for comparing numeric values,
  //modulo functions, which are assumed to be equal
  approxEquals : function(a, b, diff) {
    if (Ext.isPrimitive(a) && Ext.isPrimitive(b)) {
      if (Ext.isNumber(a) && Ext.isNumber(b)) {
        return Math.abs(a - b) <= diff;
      } else {
        return a == b;
      }
    } else if (Ext.isObject(a) && Ext.isObject(b) ||
               Ext.isArray(a) && Ext.isArray(b)) {
      var count = 0;
      var p;
      for (p in a) {
        if (Ext.isFunction(a[p])) {
          continue;
        }
        if (!approxEquals(a[p], b[p], diff)) {
          return false;
        }
        count++;
      }
      for (p in b) {
        if (Ext.isFunction(b[p])) {
          continue;
        }
        count--;
      }
      return (count === 0);
    } else {
      return true;
    }
  },

  setValue : function(json) {
    if (!this.approxEquals(this.facade.getJSON(), json.diagram, 1E-6)) {
      this.inUpdate = true; //temporary ignore onChange events
      this.clearEditor();
      this.facade.importJSON(json.diagram);
      this.inUpdate = false;
    }
    this.showErrors(json.errors);
  },

  clearEditor : function() {
    var clearChildren = function(node) {
      while (node.hasChildNodes()) {
        node.removeChild(node.firstChild);
      }
    };

    var canvas = this.facade.getCanvas();
    canvas.children.clear();
    //Remove nodes
    canvas.nodes.clear();
    clearChildren(canvas.node.childNodes[0].childNodes[1]);
    //Remove edges
    canvas.edges.clear();
    clearChildren(canvas.node.childNodes[0].childNodes[2]);
  },

  onChange : function() {
    if (!this.inUpdate) {
      var me = this;

      me.viewport = me.findViewport();
      me.viewport.fireEvent('edit', me.taskId, me.name, me.facade.getJSON());
    }
  },

  afterRender : function() {
    var me = this;

    me.callParent(arguments);//TODO Or callSuper?

    switch(oryxState) {
      case 'loaded':
        me.buildEditor();
        break;

      case 'loading':
        waitingForOryx.push(Ext.bind(me.buildEditor, me));
        break;

      case 'unloaded':
        oryxState = 'loading';
        var script = document.createElement("script");
        script.setAttribute('type', 'text/javascript');
        script.setAttribute('src', 'oryx-all.js');
        Ext.getHead().appendChild(script);
        break;
    }
  },

  buildEditor: function() {
    var url = this.stencilsetURL[0] === '/' ?
                this.stencilsetURL :
                ORYX.CONFIG.ROOT_PATH + 'stencilsets/' + this.stencilsetURL;

    this.facade = new ORYX.Editor({
      parentContainer: this,
      stencilset: {
          url: url
      }
    });

    this.facade.importJSON(this.value.diagram);

    var oryxControl = this;
    this.facade.registerOnEvent(ORYX.CONFIG.EVENT_AFTER_EXECUTE_COMMANDS,
                                function(){ oryxControl.onChange(); });
  },

  setError: function(message) { },
  setHint: function(message) { },

  showErrors: function(json) {
    this.facade.handleEvents({
      type: ORYX.Plugins.SyntaxChecker.RESET_ERRORS_EVENT
    });

    var hints = {};
    json.each(function(hint) {
      hints[hint.resourceId] = (hint.paramName !== null ?
                                hint.paramName + ' parameter: ' : ''
                               ) + hint.message;
    }.bind(this));

    this.facade.handleEvents({
      type: ORYX.Plugins.SyntaxChecker.SHOW_ERRORS_EVENT,
      errors: hints
    });
  }
});

window.onOryxResourcesLoaded = function() {
  oryxState = 'loaded';
  waitingForOryx.each(function(build){build();});
};
