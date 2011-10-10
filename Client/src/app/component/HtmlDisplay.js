Ext.define('itasks.component.HtmlDisplay',{
	extend: 'Ext.Component',
	alias: 'widget.ihtml',
	mixins: ['itasks.mixin.Editable'],
	//Make sure we have enough space at render time
	//autoEl: {tag: 'div', style: 'width: 10000px; height: 10000px;'}, 
	autoEl: {tag: 'div', style: 'width: 600px; height: 1000px;'}, 
	//Put the content first in a div to create a nice flowing box
	//and then wrap it in a span to measure the content size
	renderTpl: ['<div role="presentation"><span role="presentation">{content}</span></div>'],
	renderData: {content: null},
	initComponent: function() {
		if(!this.wrapText) {
			this.style = 'white-space: nowrap';
		}
		this.callParent(arguments);
	},
	onRender: function() {
		this.renderData.content = this.value;
		this.callParent(arguments);
	},
	afterRender: function(cmp) {
		this.callParent(arguments);

		var wrapperSize = cmp.down('span[role=presentation]').getSize(true);

		if(!this.width) {
			this.setWidth(wrapperSize.width);
			//this.width = wrapperSize.width;
		}
		if(!this.height) {
			this.setHeight(wrapperSize.height);
			//this.height = wrapperSize.height;
		}

		if (this.tooltip) {
			new Ext.ToolTip({html: this.tooltip, target: this.getEl()});
		}
	},
	setValue: function(html) {
		var el = this.el || this.getEl(),
		    wrapper = el.down('span[role=presentation]'),
		    wrapperSize;

		el.setStyle({width: '600px',height: '10000px'});
		wrapper.update(html);
		wrapperSize = wrapper.getSize(true);

		this.setSize(wrapperSize);
	}
});
