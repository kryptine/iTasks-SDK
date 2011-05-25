Ext.ns("itasks.tui");

itasks.tui.base = {
	initComponent: function() {
		this.tuiSize = {};
		this.setTuiWidth(this.width);
		this.setTuiHeight(this.height);
		delete this.width;
		delete this.height;
		
		this.extSuperclass.initComponent.apply(this,arguments);
	},
	
	setTuiWidth: function(w) {
		this.tuiSize.width	= (w == 'Auto' ? this.defaultWidth : w);
		if  (!Ext.isArray(this.tuiSize.width))
			this.tuiSize.width	= [this.tuiSize.width];
	},
	setTuiHeight: function(h) {
		this.tuiSize.height	= (h == 'Auto' ? this.defaultHeight : h);
		if  (!Ext.isArray(this.tuiSize.height))
			this.tuiSize.height	= [this.tuiSize.height];
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
		if (myW == 0 || myH == 0) {
			// hide element if width/height = 0 to prevent layout problems
			this.hide();
		}
		this.resumeEvents();
		
		return {myW: myW, myH: myH};
	},
	
	getTUISize: function() {
		var cached = this.getCache(this.id,'size');
		if (cached !== null) return cached;

		var tuiW		= this.tuiSize.width;
		var tuiH		= this.tuiSize.height;
		var size		= {};
		
		if (tuiW[0] == 'FillParent') {
			size.width	= ['Weight',tuiW[1]];
		} else {
			size.width	= ['Fixed',this.getMinTUISize().width];
		}
		
		if (tuiH[0] == 'FillParent') {
			size.height	= ['Weight',tuiH[1]];
		} else {
			size.height	= ['Fixed',this.getMinTUISize().height];
		}
		
		this.setCache(this.id,'size',size);
		return size;
	},
	
	getMinTUISize: function() {
		var cached = this.getCache(this.id,'minSize');
		if (cached !== null) return cached;
		
		var tuiW		= this.tuiSize.width;
		var tuiH		= this.tuiSize.height;
		var minSize		= {};

		if (tuiW[0] == 'Wrap' || tuiW[0] == 'FillParent' && tuiW[2] == 'ContentSize') {
			minSize.width	= this.getWidth();
		} else {
			minSize.width	= tuiW[0] == 'Fixed' ? tuiW[1] : tuiW[2][1];
		}
		minSize.width += this.getMarginsW();
		
		if (tuiH[0] == 'Wrap' || tuiH[0] == 'FillParent' && tuiH[2] == 'ContentSize') {
			minSize.height	= this.getHeight();
		} else {
			minSize.height	= tuiH[0] == 'Fixed' ? tuiH[1] : tuiH[2][1];
		}
		minSize.height += this.getMarginsH();
		
		this.setCache(this.id,'minSize',minSize);
		return minSize;
	},
	
	getMarginsW: function() {
		return (this.margins ? this.margins.left + this.margins.right : 0);
	},
	getMarginsH: function() {
		return (this.margins ? this.margins.top + this.margins.bottom : 0);
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