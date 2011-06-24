Ext.ns("itasks.tui");

itasks.tui.extendContainer = function(extSuper,overrides) {
	return itasks.util.extend(extSuper,itasks.tui.container,overrides);
};

itasks.tui.container = Ext.apply(itasks.util.clone(itasks.tui.base),{
	headerStyle: 'white-space: nowrap',
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
	},
	
	getMinTUISize: function() {
		var cached = this.getCache(this.id,'minSize');
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
		
		if (tuiW[0] == 'WrapContent' || tuiW[0] == 'FillParent' && tuiW[2] == 'ContentSize') {
			var minW = (this.orientation == 'Horizontal' ? sum : max) (function(i) {return i.minSize.width;}) + this.getFrameWidthCached() + (this.title ? 2 : 0);
			
			if (tuiW[0] == 'WrapContent' && minW < tuiW[1]) {
				minSize.width	= tuiW[1];
			} else {
				minSize.width	= minW;
			}
		} else {
			minSize.width	= tuiW[0] == 'Fixed' ? tuiW[1] : tuiW[2][1];
		}
		minSize.width += this.getMarginsW();
		
		if (tuiH[0] == 'WrapContent' || tuiH[0] == 'FillParent' && tuiH[2] == 'ContentSize') {
			var minH = (this.orientation == 'Horizontal' ? max : sum) (function(i) {return i.minSize.height;}) + this.getFrameHeightCached();
			
			if (tuiH[0] == 'WrapContent' && minH < tuiH[1]) {
				minSize.height	= tuiH[1];
			} else {
				minSize.height	= minH;
			}
		} else {
			minSize.height	= tuiH[0] == 'Fixed' ? tuiH[1] : tuiH[2][1];
		}
		minSize.height += this.getMarginsH();
		
		this.setCache(this.id,'minSize',minSize);
		return minSize;
	},
	
	getChildSizes: function() {
		var cached = this.getCache(this.id,'childSizes');
		if (cached !== null) return cached;
	
		var sizes = new Ext.util.MixedCollection();
		this.items.each(function(item) {
			var tuiSize	= item.getTUISize();
			var minSize	= item.getMinTUISize();
			
			sizes.add({
				item:		item,
				tuiSize:	tuiSize,
				minSize:	minSize
			});
		});
		
		this.setCache(this.id,'childSizes',sizes);
		return sizes;
	},
	
	getFrameWidthCached: function() {
		if(!Ext.isDefined(this.getFrameWidth))
			return 0;
		
		var type = this.xtype + '|' + Ext.isString(this.title) + '|' + this.frame + '|' + this.initialConfig.padding;
		var cached = this.getCache(type,'frameWidth',true);
		if (cached !== null) return cached;
		
		var frameWidth = this.getTUIFrameWidth();
		this.setCache(type,'frameWidth',frameWidth,true);
		return frameWidth;
	},
	getFrameHeightCached: function() {
		if(!Ext.isDefined(this.getFrameHeight))
			return 0;
			
		var type = this.xtype + '|' + Ext.isString(this.title) + '|' + this.frame + '|' + this.initialConfig.padding;
		var cached = this.getCache(type,'frameHeight',true);
		if (cached !== null) return cached;
	
		var frameHeight = this.getTUIFrameHeight();
		this.setCache(type,'frameHeight',frameHeight,true);
		return frameHeight;
	},
	
	getTUIFrameWidth: function() {
		return this.getFrameWidth();
	},
	getTUIFrameHeight: function() {
		return this.getFrameHeight();
	}
});