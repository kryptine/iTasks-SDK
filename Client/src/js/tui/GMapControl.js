Ext.ns("itasks.tui");

itasks.tui.GMapControl = itasks.tui.extendControl (Ext.Panel, {
	defaultWidth: ['Fixed', 500],
	defaultHeight: ['Fixed', 400],
	initComponent : function(){
		
		Ext.applyIf(this,
			{ border: false
			, scope: this
			, displayedMarkers : new Array()
			});
        itasks.tui.control.initComponent.apply(this,arguments);
	
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	update : function (data) {
		//Update map options
		this.gmap.setOptions(this.getOptions(data));
		
		//Update markers (can be implemented nicer)
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
		options.draggableCursor = "default";
		
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
			cmp.fireEvent('tuichange', cmp.taskId, cmp.name, value);
		};
		var lclickEventHandler = function(event){
			var ll = event.latLng
			
			var value = {
				event 	: "LEFTCLICK",
				source 	: "MAP",
				point	: [ll.lat(),ll.lng()]
			}
			cmp.fireEvent('tuichange', cmp.taskId, cmp.name, value);
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
			var cmp = this;
					
			var markerObj = new google.maps.Marker({
				map : mapObj,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1]),
				title : this.markers[i].title,
				draggable : this.markers[i].draggable
			});
				
			if(this.markers[i].infoWindow) {
				var infoObj = new google.maps.InfoWindow({
					content : this.markers[i].infoWindow.content
				});	
			
				var clickHandler = function(mapObj,markerObj) {
					return function() {
						infoObj.open(mapObj,markerObj);
					}
				}	
				google.maps.event.addListener(markerObj,'click',clickHandler(mapObj,markerObj));
			}
			
			if(this.markers[i].draggable) {
				var handler = function(markerId) { return function(event) {
					
					var value = {index: markerId, point : [event.latLng.lat(),event.latLng.lng()]}
					
					cmp.fireEvent('tuichange', cmp.taskId, cmp.name, value);
				};};
				
				google.maps.event.addListener(markerObj,'dragend', handler(i));
			}
			
			this.displayedMarkers[i] = markerObj;
		}
			
	}
});

Ext.reg("itasks.tui.GMapControl",itasks.tui.GMapControl);