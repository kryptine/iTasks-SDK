Ext.ns("itasks.tui");

itasks.tui.base = {
	initComponent: function() {
		this.extSuperclass.initComponent.apply(this,arguments);
	},
	doTUILayout: function(fillW,fillH) {
	
		var minSize	= this.getMinTUISize();	
		var myW,MyH;
		
		if(this.hflex > 0 && Ext.isDefined(fillW) && fillW >= minSize.width) {
			myW = fillW - this.getMarginsW(); 
		} else if(this.width > 0) {
			myW = this.width;
		} else { // wrap or fillParent with insufficient space
			myW = minSize.width - this.getMarginsW();
		}
		if(this.vflex > 0 && Ext.isDefined(fillH) && fillH >= minSize.height) {
			myH = fillH - this.getMarginsH(); 
		} else if(this.height > 0) {
			myH = this.height;
		} else { // wrap or fillParent with insufficient space
			myH = minSize.height - this.getMarginsH();
		}
		
		this.setSize(myW,myH);

		return {myW: myW, myH: myH};
	},
	
	setSize: function(w,h) {
		this.suspendEvents();
		this.extSuperclass.setSize.call(this,w,h);
		this.resumeEvents();
	},
	
	getTUISize: function() {
		var cached = this.getCache(this.id,'size');
		if (cached !== null) return cached;

		var size		= {};
		
		if(this.hflex > 0) {
			size.width = ['Weight',this.hflex];
		} else {
			size.width = ['Fixed',this.getMinTUISize().width];
		}
		if(this.vflex > 0) {
			size.height = ['Weight', this.vflex];
		} else {
			size.height = ['Fixed',this.getMinTUISize().height];
		}
	
		this.setCache(this.id,'size',size);

		return size;
	},
	
	getMinTUISize: function() {
		var cached = this.getCache(this.id,'minSize');
		if (cached !== null) return cached;
		
		var minSize		= {};

		if(this.hwrap) {
			var minW = this.getContentWidth();
			if(minW < (this.minWidth || 0)) {
				minSize.width = this.minWidth;
			} else {
				minSize.width = minW;
			}
		} else {
			minSize.width = (this.minWidth > 0 ) ? this.minWidth : (this.width || 0);
		}
		minSize.width += this.getMarginsW();
		
		if(this.vwrap) {
			var minH = this.getContentHeight();
			if(minH < (this.minHeight || 0)) {
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
	
	getContentHeight: function() {
		return this.getHeight();
	},
	getContentWidth: function() {
		return this.getWidth();
	},
	
	getMarginsW: function() {
		return this.margins ? this.margins.left + this.margins.right : 0;
	},
	getMarginsH: function() {
		return this.margins ? this.margins.top + this.margins.bottom : 0;
	},
	
	getCache: function(id,key,perm) {
		var cache = perm ? itasks.tui.permCache : itasks.tui.cache;
		var x = cache[id];
		if (Ext.isDefined(x) && Ext.isDefined(x[key])) {
			return x[key];
		} else {
			return null;
		}
	},
	setCache: function(id,key,v,perm) {
		var cache = perm ? itasks.tui.permCache : itasks.tui.cache;
		
		if (!Ext.isDefined(cache[id]))
			cache[id] = {};
			
		cache[id][key] = v;
	}
};