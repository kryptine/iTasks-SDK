Ext.ns("itasks");

itasks.GMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.applyIf(this,
			{ url: "/handlers/work/tab"
			, zoom: 15
			, center : [51.824118, 5.868174]
			, mapType : "ROADMAP"
            , border: false
			, autoHeight: false
			, height: 400
			, width: 500
			, scope: this
			, displayedMarkers : new Array()
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		
		this.show();
	},
	
	setValue : function(_data){
		var data = Ext.decode(_data);
		this.markers = data.markers;
		this.addMarkers();
	},
	
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},
	
	buildMap : function(){
		var options = 
			{ center : new google.maps.LatLng(this.center[0],this.center[1])
			, zoom: this.zoom
			, mapTypeId : this.getMapType(this.mapType)
		}
		
		this.gmap = new google.maps.Map(this.body.dom, options);

		this.addMarkers()
		
		var parent = this;
		
		var mvcEventHandler = function(){
			
			var ll = parent.gmap.getCenter();
			var zm = parent.gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : parent.gmap.getMapTypeId().toUpperCase()
			}
			
			var ct = parent.findParentByType("itasks.task-form");
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
			
			var ct = parent.findParentByType("itasks.task-form");
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();			
		}
				
		if(this.editor){
			google.maps.event.addListener(this.gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'idle', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'click', lclickEventHandler);
		}
	
	},
	
	afterRender : function(){
		itasks.GMapPanel.superclass.afterRender.call(this);  
		
		var parent = this;
		
		if(itasks.app.googleMapsLoaded){
			parent.buildMap();
		}else{
			Ext.Ajax.remoteRequest({
				url : 'http://maps.google.com/maps/api/js',
				method : 'GET',
				scriptTag: true,
				params : {
					sensor : false
				},
				success : function(response){
					parent.buildMap();
					itasks.app.googleMapsLoaded = true
				},
				failure : function(response){
					Ext.Msg.alert('Failed to load Google maps API');
				},
			});
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