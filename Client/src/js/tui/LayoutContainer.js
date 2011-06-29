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
		this.sumW = this.orientation == 'Horizontal';
		this.sumH = this.orientation == 'Vertical';
		
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
	},
	
	doTUILayout: function(fillW,fillH) {
		var myS = itasks.tui.base.doTUILayout.apply(this,arguments);
		
		var totalFillW	= myS.myW - this.getFrameWidthCached()	- (this.title ? 2 : 0);
		var totalFillH	= myS.myH - this.getFrameHeightCached()	- (this.title ? 1 : 0);
		var sizes		= this.getChildSizes();
		var horizontal	= this.orientation == 'Horizontal';
		
		if (horizontal) {
			do {
				var changedFillParent = false;
				var sumWeights	= 0;
				
				sizes.each(function(s) {
					if (Ext.isDefined(s.tuiSize.width)) {
						var w = s.tuiSize.width;
						if (w[0] == 'Fixed') {
							totalFillW -= w[1];
							delete s.tuiSize.width;
						} else {
							sumWeights += w[1];
						}
					}
				});
				sizes.each(function(s) {
					if (Ext.isDefined(s.tuiSize.width)) {
						var w = s.tuiSize.width;
						var fillW = w[1] / sumWeights * totalFillW;
						
						if (fillW < s.minSize.width) {
							s.tuiSize.width = ['Fixed',s.minSize.width];
							delete s.fillW;
							changedFillParent = true;
							return false; // stop iteration
						}

						s.fillW = fillW;
					}
				});
			} while (changedFillParent);
		} else {
			do {
				var changedFillParent = false;
				var sumWeights	= 0;
				
				sizes.each(function(s) {
					if (Ext.isDefined(s.tuiSize.height)) {
						var h = s.tuiSize.height;
						if (h[0] == 'Fixed') {
							totalFillH -= h[1];
							delete s.tuiSize.height;
						} else {
							sumWeights += h[1];
						}
					}
				});
				sizes.each(function(s) {
					if (Ext.isDefined(s.tuiSize.height)) {
						var h = s.tuiSize.height;
						var fillH = h[1] / sumWeights * totalFillH;
						
						if (fillH < s.minSize.height) {
							s.tuiSize.height = ['Fixed',s.minSize.height];
							delete s.fillH;
							changedFillParent = true;
							return false; // stop iteration
						}
						
						s.fillH = fillH;
					}
				});
			} while (changedFillParent);
		}
		
		sizes.each(function(s) {
			s.item.doTUILayout(
				horizontal ? s.fillW : totalFillW,
				horizontal ? totalFillH : s.fillH
			);
		});
	}
});

Ext.reg('itasks.tui.LayoutContainer',itasks.tui.LayoutContainer);