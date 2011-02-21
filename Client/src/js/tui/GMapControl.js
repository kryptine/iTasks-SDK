Ext.ns("itasks.tui");

itasks.tui.GMapControl = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.applyIf(this,
			{ border: false
			, height: 400
			, scope: this
			, displayedMarkers : new Array()
			});

		itasks.tui.GMapControl.superclass.initComponent.apply(this,arguments);
	
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	update : function (data) {
		//Update map options
		this.gmap.setOptions(this.getOptions(data));
		
		//Update markers
		this.markers = data.markers;
		this.addMarkers();
	},
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},
	getOptions : function(data) {
		var options = data.options;
		options.center = new google.maps.LatLng(data.center[0],data.center[1]);
		options.mapTypeId = this.getMapType(data.mapType);
		
		return options;
	},
	
	buildMap: function(){
		
		this.gmap = new google.maps.Map(this.body.dom, this.getOptions(this));

		this.addMarkers();
	
		var cmp = this;
			
		var mvcEventHandler = function(){
			var ll = cmp.gmap.getCenter();
			var zm = cmp.gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : cmp.gmap.getMapTypeId().toUpperCase()
			}
			cmp.fireEvent('tuichange', cmp.name, Ext.encode(value));
		};
		var lclickEventHandler = function(event){
			var ll = event.latLng
			
			var value = {
				event 	: "LEFTCLICK",
				source 	: "MAP",
				point	: [ll.lat(),ll.lng()]
			}
			cmp.fireEvent('tuichange', cmp.name, Ext.encode(value));
		};
				
		if(this.editor){
			google.maps.event.addListener(this.gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'dragend', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'zoom_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'click', lclickEventHandler);
		}
	},
	
	afterRender : function(){
		itasks.tui.GMapControl.superclass.afterRender.call(this);

		switch(itasks.app.googleMapsState) {
			case 'loaded':
				this.buildMap();
				break;
			case 'unloaded':
				itasks.app.googleMapsState = 'loading';
				Ext.Ajax.remoteRequest({
					url : 'http://maps.google.com/maps/api/js?v=3.3',
					method : 'GET',
					scriptTag: true,
					params : {
						sensor : false
					},
					success : function(response){
						itasks.app.googleMapsState = 'loaded';
						itasks.app.waitingForGoogleMaps.each(function(build){build();});
					},
					failure : function(response){
						itasks.app.googleMapsState = 'unloaded';
						Ext.Msg.alert('Failed to load Google maps API');
					}
				});
			case 'loading':
				itasks.app.waitingForGoogleMaps.addAll([this.buildMap.createDelegate(this)]);
		}
	},
	
	addMarkers : function (){
		var i=0;
		for(i=0; i<this.displayedMarkers.length; i++){
			this.displayedMarkers[i].setMap(null);
		}
		
		this.displayedMarkers = new Array();
		
		for(i=0; i<this.markers.length; i++){
			
			var mapObj = this.gmap;
					
			var markerObj = new google.maps.Marker({
				map : mapObj,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1]),
				title : this.markers[i].title
			});
			
			if(this.markers[i].infoWindow) {
				var infoObj = new google.maps.InfoWindow({
					content : this.markers[i].infoWindow.content
				});	
			
				google.maps.event.addListener(markerObj,'click',function() {
					infoObj.open(mapObj,markerObj);
				});
			}
			this.displayedMarkers[i] = markerObj;
		}
			
	}
});

Ext.reg("itasks.tui.GMapControl",itasks.tui.GMapControl);