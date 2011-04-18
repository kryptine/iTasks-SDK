Ext.ns('itasks.tui');

itasks.tui.LayoutContainer = Ext.extend(Ext.Panel,{
	initComponent: function(){
		this.unstyled = !this.title && !this.frame;
		var width = this.width;
		delete this.width;

		switch (Ext.isArray(width) ? width[0] : width) {
			case 'Auto':
				this.width = 700;
				break;
			case 'FillParent':
				switch (this.orientation) {
					case 'Vertical':
						this.layout = 'auto';
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
						break;
				}
				break;
			case 'Wrap':
				this.listeners = {afterlayout: function(c) {
					var maxWidth = 0;
					c.items.each(function(i) {
						if (Ext.isArray(i.initialConfig.width) && i.initialConfig.width[0] == 'FillParent')
							return;
						
						var w = i.getOuterSize().width;
						if (w > maxWidth)
							maxWidth = w;
					});
					
					maxWidth += c.getFrameWidth();
					if (c.getWidth() != maxWidth) {
						c.setWidth(maxWidth);
					}
				}};
				break;
			case 'Fixed':
				this.width = width[1];
				break;
		}

		itasks.tui.LayoutContainer.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.LayoutContainer',itasks.tui.LayoutContainer);
