Ext.ns('itasks.tui');

itasks.tui.LayoutContainer = Ext.extend(Ext.Panel,{
	autoScroll: true,
	initComponent: function(){
		if (this.padding) {
			this.style = "padding: " + this.padding + "px;";
			delete this.padding;
		}
		this.unstyled		= !this.title && !this.frame;
		this.tuiSize		= {};
		this.tuiSize.width	= (this.width == 'Auto' ? ['Fixed',700] : this.width);
		this.tuiSize.height	= (this.height == 'Auto' ? ['Wrap'] : this.height);
		if  (!Ext.isArray(this.tuiSize.width))
			this.tuiSize.width	= [this.tuiSize.width];
		if  (!Ext.isArray(this.tuiSize.height))
			this.tuiSize.height	= [this.tuiSize.height];
		delete this.width;
		delete this.height;
		
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

		itasks.tui.LayoutContainer.superclass.initComponent.apply(this,arguments);
	},
	
	doTUILayout: function(fillW,fillH) {
		var tuiW	= this.tuiSize.width;
		var tuiH	= this.tuiSize.height;
		var minSize	= this.getMinTUISize();
		
		if (tuiW[0] == 'FillParent' && Ext.isDefined(fillW) && fillW >= minSize.width) {
			var myW = fillW - this.getMarginsW();
		} else if (tuiW[0] == 'Fixed') {
			var myW = tuiW[1];
		} else { // wrap or fillParent with insufficient space
			var myW = minSize.width - this.getMarginsW();
		}
		
		if (tuiH[0] == 'FillParent' && Ext.isDefined(fillH) && fillH >= minSize.height) {
			var myH = fillH - this.getMarginsH();
		} else if (tuiH[0] == 'Fixed') {
			var myH = tuiH[1];
		} else { // wrap or fillParent with insufficient space
			var myH = minSize.height - this.getMarginsH();
		}
		
		this.suspendEvents();
		this.setSize(myW,myH);
		this.resumeEvents();
		
		var totalFillW	= myW - this.getFrameWidthCached()	- (this.title ? 2 : 0);
		var totalFillH	= myH - this.getFrameHeightCached()	- (this.title ? 1 : 0);
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
			if (Ext.isFunction(s.item.doTUILayout)) {
				s.item.doTUILayout(
					horizontal ? s.fillW : totalFillW,
					horizontal ? totalFillH : s.fillH
				);
			}
		});
	},
	
	getTUISize: function() {
		var cached = this.getCache('size');
		if (cached !== null) return cached;
		
		var tuiW		= this.tuiSize.width;
		var tuiH		= this.tuiSize.height;
		var size		= {};
		
		switch (tuiW[0]) {
			case 'Wrap':
				size.width	= ['Fixed',this.getMinTUISize().width];
				break;
			case 'FillParent':
				size.width	= ['Weight',tuiW[1]];
				break;
			case 'Fixed':
				size.width	= ['Fixed',tuiW[1]];
				break;
		}
		switch (tuiH[0]) {
			case 'Wrap':
				size.height	= ['Fixed',this.getMinTUISize().height];
				break;
			case 'FillParent':
				size.height	= ['Weight',tuiH[1]];
				break;
			case 'Fixed':
				size.height	= ['Fixed',tuiH[1]];
				break;
		}
		
		this.setCache('size',size);
		return size;
	},
	
	getMinTUISize: function() {
		var cached = this.getCache('minSize');
		if (cached !== null) return cached;
		
		var tuiW		= this.tuiSize.width;
		var tuiH		= this.tuiSize.height;
		var minSize		= {};
		var childSizes	= this.getChildSizes();
		var max			= function(get) {
			var max = 0;
			childSizes.each(function(i) {
				var v = get(i);
				if (v > max) max = v;
			});
			return max;
		};
		var sum = function(get) {
			var sum = 0;
			childSizes.each(function(i) {
				sum += get(i);
			});
			return sum;
		};
		
		if (tuiW[0] == 'Wrap' || tuiW[0] == 'FillParent' && tuiW[2] == 'ContentSize') {
			minSize.width	= (this.orientation == 'Horizontal' ? sum : max) (function(i) {return i.minSize.width;}) + this.getFrameWidthCached() + (this.title ? 2 : 0);
		} else {
			minSize.width	= tuiW[0] == 'Fixed' ? tuiW[1] : tuiW[2][1];
		}
		minSize.width += this.getMarginsW();
		
		if (tuiH[0] == 'Wrap' || tuiH[0] == 'FillParent' && tuiH[2] == 'ContentSize') {
			minSize.height	= (this.orientation == 'Horizontal' ? max : sum) (function(i) {return i.minSize.height;}) + this.getFrameHeightCached();
		} else {
			minSize.height	= tuiH[0] == 'Fixed' ? tuiH[1] : tuiH[2][1];
		}
		minSize.height += this.getMarginsH();
		
		this.setCache('minSize',minSize);
		return minSize;
	},
	
	getChildSizes: function() {
		var cached = this.getCache('childSizes');
		if (cached !== null) return cached;
	
		var sizes = new Ext.util.MixedCollection();
		this.items.each(function(item) {
			if (Ext.isFunction(item.getTUISize) && Ext.isFunction(item.getMinTUISize)) {
				var tuiSize	= item.getTUISize();
				var minSize	= item.getMinTUISize();
			} else {
				var s		= item.getSize();
				if(item.margins) {
					s.width		+= (item.margins.left + item.margins.right);
					s.height	+= (item.margins.top + item.margins.bottom);
				}
				var tuiSize	= {width: ['Fixed',s.width],	height: ['Fixed',s.height]};
				var minSize	= {width: s.width,				height: s.height};
			}
			
			sizes.add({
				item:		item,
				tuiSize:	tuiSize,
				minSize:	minSize
			});
		});
		
		this.setCache('childSizes',sizes);
		return sizes;
	},
	
	getMarginsW: function() {
		return (this.margins ? this.margins.left + this.margins.right : 0);
	},
	getMarginsH: function() {
		return (this.margins ? this.margins.top + this.margins.bottom : 0);
	},
	getFrameWidthCached: function() {
		if(!Ext.isDefined(this.cachedFrameWidth))
			this.cachedFrameWidth = this.getFrameWidth();
			
		return this.cachedFrameWidth;
	},
	getFrameHeightCached: function() {
		if(!Ext.isDefined(this.cachedFrameHeight))
			this.cachedFrameHeight = this.getFrameHeight();
			
		return this.cachedFrameHeight;
	},
	
	getCache: function(key) {
		var x = itasks.tui.cache[this.id];
		if (Ext.isDefined(x) && Ext.isDefined(x[key])) {
			return x[key];
		} else {
			return null;
		}
	},
	setCache: function(key,v) {
		var x = itasks.tui.cache[this.id];
		if (!Ext.isDefined(x))
			itasks.tui.cache[this.id] = {};
			
		itasks.tui.cache[this.id][key] = v;
	}
});

Ext.reg('itasks.tui.LayoutContainer',itasks.tui.LayoutContainer);