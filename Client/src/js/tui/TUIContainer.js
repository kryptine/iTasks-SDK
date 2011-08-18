Ext.ns("itasks.tui");

itasks.tui.extendContainer = function(extSuper,overrides) {
	return itasks.util.extend(extSuper,itasks.tui.container,overrides);
};

itasks.tui.container = Ext.apply(itasks.util.clone(itasks.tui.base),{
	headerStyle: 'white-space: nowrap',
	
	getMinTUISize: function() {
		var cached = this.getCache(this.id,'minSize');
		if (cached !== null) return cached;
	
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
		
		if(this.hwrap) {
			var minW = (this.sumW ? sum : max) (function(i) {return i.minSize.width;}) + this.getFrameWidthCached() + (this.title ? 2 : 0);
			
			if( minW < (this.minWidth || 0)) {
				minSize.width = this.minWidth;
			} else {
				minSize.width = minW;
			}
		} else {
			minSize.width = (this.minWidth > 0 ) ? this.minWidth : (this.width || 0);
		}
		minSize.width += this.getMarginsW();
		
		if(this.vwrap) {
			var minH = (this.sumH ? sum : max) (function(i) {return i.minSize.height;}) + this.getFrameHeightCached();
		
			if( minH < (this.minHeight || 0)) {
				minSize.height = this.minHeight;
			} else {
				minSize.height = minH;
			}
		} else {
			minSize.height = (this.minHeight > 0 ) ? this.minHeight : (this.height || 0);
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