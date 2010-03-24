Ext.ns("itasks");

itasks.GMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.applyIf(this,
			{ url: "/handlers/work/tab"
			, border: false
			, autoHeight: false
			, scope: this
			, displayedMarkers : new Array()
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		this.show();
	},
	
	setValue : function(_data){
		var data = Ext.decode(_data);
		
		if(this.getWidth() != data.width || this.getHeight() != data.height) {
			this.setSize(data.width, data.height);
			this.buildMap();
		}
		this.gmap.setOptions(this.getOptions(data));
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
	
	buildMap : function(){
		this.gmap = new google.maps.Map(this.body.dom, this.getOptions(this));

		this.addMarkers();
		
		var parent = this;
		
		var mvcEventHandler = function(){
			var ll = parent.gmap.getCenter();
			var zm = parent.gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : parent.gmap.getMapTypeId().toUpperCase()
			}
			
			var ct = parent.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();
		}
		
		var lclickEventHandler = function(event){
			var ll = event.latLng
			
			var value = {
				event 	: "LEFTCLICK",
				source 	: "MAP",
				point	: [ll.lat(),ll.lng()]
			}
			
			var ct = parent.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();			
		}
				
		if(this.editor){
			google.maps.event.addListener(this.gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'dragend', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'zoom_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'click', lclickEventHandler);
		}
	},
	
	afterRender : function(){
		itasks.GMapPanel.superclass.afterRender.call(this);
		
		switch(itasks.app.googleMapsState) {
			case 'loaded':
				this.buildMap();
				break;
			case 'unloaded':
				itasks.app.googleMapsState = 'loading';
				Ext.Ajax.remoteRequest({
					url : 'http://maps.google.com/maps/api/js',
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
						
			var markerObj = new google.maps.Marker({
				map : this.gmap,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1])
			});	

			this.displayedMarkers[i] = markerObj;
		}		
	}
});

Ext.reg('itasks.gmappanel', itasks.GMapPanel);