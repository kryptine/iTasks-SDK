Ext.ns("itasks.tui");

itasks.tui.SourceCodeControl = Ext.extend(Ext.form.TextArea, {
    language: 'txt',
    codeMirrorPath: 'CodeMirror-0.67', // should be path to code mirror on your server!
    initComponent: function() {
        if (this.codeMirrorPath === null) {
            throw 'itasks.tui.SourceCodeControl: codeMirrorPath required';
        }
        this.initialized = false;
        itasks.tui.SourceCodeControl.superclass.initComponent.apply(this, arguments);
		this.on('change', function(){console.log(123);});
        this.addEvents('initialize');
		this.addEvents('update');
        this.on({
            resize: function(ta, width, height) {
                var el = Ext.select('.'+this.id, true);
                if (el){
                //width -= 35;
                    
                            for (var i=0; i< el.elements.length; i++)
                            {
                            el.elements[i].setSize(width, height);
                            }
                }
                if (el) {
                    width -= 35;
                    /* Doesn't work in ie.
                    el.elements.forEach(function(e) {
                        e.setSize(width, height);
                    });*/
                }
            },
            afterrender: function() {
                var parser, stylesheet;
                switch (this.language.toLowerCase()) {
                    case 'css':
                        parser = 'parsecss.js';
                        stylesheet = this.codeMirrorPath+'/css/csscolors.css';
                        break;
                    case 'js':
                        parser = ['tokenizejavascript.js', 'parsejavascript.js'];
                        stylesheet = this.codeMirrorPath+'/css/jscolors.css';
                        break;
					case 'clean':
                        parser = ['tokenizeclean.js', 'parseclean.js'];
                        stylesheet = this.codeMirrorPath+'/css/cleancolors.css';
                        break;
                    case 'php':
                        parser = [
                            "parsexml.js",
                            "parsecss.js",
                            "tokenizejavascript.js",
                            "parsejavascript.js",
                            "../contrib/php/js/tokenizephp.js",
                            "../contrib/php/js/parsephp.js",
                            "../contrib/php/js/parsephphtmlmixed.js"
                        ];
                        stylesheet = [
                            this.codeMirrorPath+'/css/xmlcolors.css',
                            this.codeMirrorPath+'/css/jscolors.css',
                            this.codeMirrorPath+'/css/csscolors.css',
                            this.codeMirrorPath+'/contrib/php/css/phpcolors.css'
                        ];
                        break;
                    case 'htm':
                    case 'html':
                    case 'xml':
                        parser = 'parsexml.js';
                        stylesheet = 'xmlcolors.css';
                        break;
                    default:
                        parser = 'parsedummy.js';
                        stylesheet = '';
                        break;
                    
                }
                var me = this;
                                (function() {
                me.codeEditor = new CodeMirror.fromTextArea(me.id, {
                    parserfile: parser,
                    stylesheet: stylesheet,
                    path: me.codeMirrorPath+'/js/',
                    textWrapping: false,
                    lineNumbers: true,
                    iframeClass: 'codemirror-iframe '+me.id,
                    content: this.value,
                    initCallback: function() {
                        me.initialized = true;
						me.setValue(me.value);
                        me.fireEvent('initialize', true);
                    },
					onChange: function() {
						var sendUpdate = new Ext.util.DelayedTask(function() {
							me.lastValue = me.getValue();
							me.fireEvent('update');
						});
						
						sendUpdate.delay(750);
					},
					width: '700px',
					height: '300px'
                });
                            }).defer(100);                
            }
        });
    },
    getValue: function() {
        if (this.initialized) {
            return this.codeEditor.getCode();
        }
        return this.value;    
    },
    setValue: function(v) {
        if (this.initialized && v != this.lastValue) {
            this.codeEditor.setCode(v);
        }
    },
    validate: function() {
        this.getValue();
        itasks.tui.SourceCodeControl.superclass.validate.apply(this, arguments);
    }
});

Ext.reg('itasks.tui.SourceCode', itasks.tui.SourceCodeControl);