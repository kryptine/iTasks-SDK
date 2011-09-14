Ext.define('itasks.extension.GoogleMap',{
	extend: 'Ext.Component',
	alias: 'widget.igooglemap',
	mixins: ['itasks.mixin.Editable'],
	statics: {
		googleApiStatus: 'unloaded',
		googleApiWaiters: [] // Track components that wait for the api to load
	},
	map: null,
	hflex: true,
	height: 300,
	
	afterRender: function() {
		var me = this;
		
		me.callParent(arguments);
		
		switch(me.self.googleApiStatus) {
			case 'loaded':
				me.setupMap();
				break;
			case 'loading':
				this.self.googleApiWaiters.push(Ext.bind(me.setupMap,me));
				break;
			case 'unloaded':
				me.self.googleApiStatus = 'loading';
				me.self.googleApiWaiters.push(Ext.bind(me.setupMap,me));
				
				//Setup google maps callback handler
				window.googlemapsready = Ext.bind(me.apiLoaded,me);
				//Add google API to head tag
				var head = document.getElementsByTagName('head')[0];
				var tag = document.createElement('script');
				tag.type = 'text/javascript';
				tag.src = 'http://maps.google.com/maps/api/js?v=3.3&sensor=false&callback=googlemapsready';
				head.appendChild(tag);
				break;
		}
	},
	apiLoaded: function() {
		this.setupMap();
	},
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},
	getOptions : function(data) {
		var options = data.options;
		options.center = new google.maps.LatLng(data.center[0],data.center[1]);
		options.mapTypeId = this.getMapType(data.mapType);
		options.draggableCursor = "default";
		
		return options;
	},
	setupMap: function() {
		this.map = new google.maps.Map(this.el.dom, this.getOptions(this));
	},
	update: function(def) {
		//TODO: impelement for incremental updates of maps
	}
	
});