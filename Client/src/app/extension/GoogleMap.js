Ext.define('itasks.extension.GoogleMap',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks-googlemap',
	mixins: ['itasks.mixin.Editable'],
	statics: {
		googleApiStatus: 'unloaded',
		googleApiWaiters: [] // Track components that wait for the api to load
	},
	initComponent: function() {

		this.map = null;
		this.displayedMarkers = [];

		if(!this.width && !this.hflex) {
			this.hflex = 1;	
			this.minWidth = 400;
		}
		if(!this.height && !this.vflex) {		
			this.vflex = 1;
			this.minHeight = 300;
		}
		this.callParent(arguments);
	},
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
		console.log("Google Maps API Loaded");
		this.self.googleApiStatus = 'loaded';
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
		console.log("Setting up map");
		console.log(this.el.dom.innerHTML);
		if(!this.map) {
			this.map = new google.maps.Map(this.el.dom, this.getOptions(this));
			this.addMarkers();
		}
	},
	addMarkers: function() {
 		var	me = this,
			map = this.map,
			marker, infoWindow, clickHandler, dragHandler, i;

		for(i=0; i<this.displayedMarkers.length; i++) {
			this.displayedMarkers[i].setMap(null);
		}
        
		this.displayedMarkers = new Array();
        
		for(i=0; i<this.markers.length; i++) {
            
			marker = new google.maps.Marker({
				map : map,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1]),
				title : this.markers[i].title,
				draggable : this.markers[i].draggable,
				icon: this.markers[i].icon ? ('/googlemap-icons/' + this.markers[i].icon + '.png') : null
			});
                
			if(this.markers[i].infoWindow) {
                		infoWindow = new google.maps.InfoWindow({
					content : this.markers[i].infoWindow.content
				}); 
            
				clickHandler = function(map,marker) {
					return function() {infoWindow.open(map,marker);};
				};

				google.maps.event.addListener(marker,'click',clickHandler(map,marker));
			}
            
			if(this.markers[i].draggable) {
				dragHandler = function(markerId) { return function(e) {
                    			me.fireEvent('edit', me.taskId, me.name, {index: markerId, point : [e.latLng.lat(),e.latLng.lng()]});
                		};};
                
				google.maps.event.addListener(marker,'dragend', dragHandler(i));
			}
            
			this.displayedMarkers[i] = marker;
		}
	},
    afterComponentLayout: function() {
    	//Workaround for Google maps API which does not handle resizing divs very well 
    	if(this.map) {
    		google.maps.event.trigger(this.map, 'resize');
    	}
    	this.callParent(arguments);
    },
	onDestroy: function() {
		delete this.map;
	},
	update: function(def) {
		console.log("Unimplemented map update");
		//TODO: impelement for incremental updates of maps
	}
	
});
