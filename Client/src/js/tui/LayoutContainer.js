Ext.ns('itasks.tui');

itasks.tui.LayoutContainer = itasks.tui.extendContainer(Ext.Panel,{
	autoScroll: true,
	defaultWidth: ['WrapContent',0],
	defaultHeight: ['WrapContent',0],
	initComponent: function(){
		if (this.padding) {
			this.style = "padding: " + this.padding + "px;";
			delete this.padding;
		}
		this.unstyled = !this.title && !this.frame;
		
		switch (this.orientation) {
			case 'Vertical':
				this.layout = {type: 'vbox'};
				
				switch (this.vGravity) {
					case 'VGTop':
						this.layout.pack = 'start';
						break;
					case 'VGCenter':
						this.layout.pack = 'center';
						break;
					case 'VGBottom':
						this.layout.pack = 'end';
					break;
				}
				switch (this.hGravity) {
					case 'HGLeft':
						this.layout.align = 'top';
						break;
					case 'HGCenter':
						this.layout.align = 'center';
						break;
					case 'HGRight':
						alert("not implemented");
					break;
				}
				break;
			case 'Horizontal':
				this.layout = {type: 'hbox'};
				
				switch (this.hGravity) {
					case 'HGLeft':
						this.layout.pack = 'start';
						break;
					case 'HGCenter':
						this.layout.pack = 'center';
						break;
					case 'HGRight':
						this.layout.pack = 'end';
					break;
				}
				switch (this.vGravity) {
					case 'VGTop':
						this.layout.align = 'top';
						break;
					case 'VGCenter':
						this.layout.align = 'middle';
						break;
					case 'VGBottom':
						alert("not implemented");
					break;
				}
				break;
		}

		itasks.tui.base.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.LayoutContainer',itasks.tui.LayoutContainer);