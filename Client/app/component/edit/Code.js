/**
* Lightweight wrapper of codemirror editor
* Loosely based on mzExt codemirror component by Adrian Teodorescu but optimized for use in iTasks client framework
* instead of reusable extjs component.
*/

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

Ext.define('itwc.component.edit.Code',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_edit_code',
	mixins: ['itwc.component.edit.Editable'],
	componentLayout: 'itwc_edit_code',
	
	width: 'flex',
	minWidth: 400,
	height: 'flex',
	minHeight: 100,
	
	baseCls: Ext.baseCSSPrefix + 'html-editor-wrap',
	
	childEls: ['editorEl'/*,'wrapperEl'*/],
	renderTpl:	[//'<div id="{cmpId}-wrapperEl" class="{editorWrapCls}">'
       			,'<div id="{cmpId}-editorEl" class="{editorCls}" style="{size}"></div>'
       			//,'</div>'
       			],
	
	initComponent: function() {
		var me = this;
		
		me.renderData['cmpId'] = this.id;
        me.renderData['editorCls'] = Ext.baseCSSPrefix + 'codemirror';
		me.renderData['editorWrapCls'] = Ext.baseCSSPrefix + 'html-editor-wrap',
		me.renderData['size'] = 'height:100px; width:100%';
	
		me.callParent(arguments);
			
		//me.initEditable();
		
		//Refresh codemirror code on resize
		me.on('resize', function() {
			if (me.editor) {
				me.editor.refresh();
			}
		}, me);
	},
	onRender: function() {
		this.callParent(arguments);
		this.initEditor();
	},
	initEditor: function() {
		var me = this,
			css;
		
		me.editor = CodeMirror(me.editorEl, {
			value: me.value,
			lineNumbers: true,
			onChange: function(editor, change){
				me.viewport = me.findViewport();	
				me.viewport.fireEvent('edit',me.taskId,me.getEditorId(),editor.getValue());
			}
		});

		// change the codemirror css
		css = Ext.util.CSS.getRule('.CodeMirror');
		if(css){
			css.style.height = '100%';
			css.style.position = 'relative';
			css.style.overflow = 'hidden';
		}
		css = Ext.util.CSS.getRule('.CodeMirror-Scroll');
		if(css){
			css.style.height = '100%';
		}
	},
	getEditorValue: function() {
		me.editor.getValue();
	}
});
