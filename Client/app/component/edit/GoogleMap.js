Ext.define('itwc.component.edit.GoogleMap',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_edit_googlemap',
	mixins: ['itwc.component.edit.Editable'],

	width: 'flex',
	minWidth: 400,
	height: 'flex',
	minHeight: 300,

	statics: {
		googleApiStatus: 'unloaded',
		googleApiWaiters: [] // Track components that wait for the api to load
	},
	initComponent: function() {

		this.map = null;
		this.displayedMarkers = [];

		this.callParent(arguments);
		this.initEditable();
	},
	afterRender: function() {
		var me = this;
		
		me.callParent(arguments);
			
		switch(me.self.googleApiStatus) {
			case 'loaded':
				me.setupMap();
				break;
			case 'loading':
				me.self.googleApiWaiters.push(Ext.bind(me.setupMap,me));
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
		this.self.googleApiStatus = 'loaded';
		this.setupMap();
	},
	getMapType : function (mapType){
		return google.maps.MapTypeId[mapType];
	},
	getOptions : function() {
		var me = this, options = me.options;
		
		options.center = new google.maps.LatLng(me.center[0],me.center[1]);
		options.mapTypeId = this.getMapType(me.mapType);
		options.draggableCursor = "default";
		
		return options;
	},
	setupMap: function() {
		var me = this, i;
		
		if(!me.map && me.el) {
			
			me.map = new google.maps.Map(me.el.dom, me.getOptions());

            for (i = 0; i < me.markers.length; i++) {
                me.addMarker(i,me.markers[i]);
            }
			
			var updatePerspective = function() {
				var center = me.map.getCenter(),
					zoom = me.map.getZoom(),
					type = me.map.getMapTypeId().toUpperCase();
			
				var e = {center : [center.lat(),center.lng()], zoom: zoom, type : type}
			
				me.lastEditNo = itwc.global.controller.sendEditEvent(me.taskId,me.editorId,e);
			};
			
			//Add perspective change
			google.maps.event.addListener(me.map,'dragend', updatePerspective);
			google.maps.event.addListener(me.map,'maptypeid_changed', updatePerspective);
			google.maps.event.addListener(me.map,'zoom_changed', updatePerspective);
		}
	},
    afterComponentLayout: function() {
		var me = this;

    	if(me.map && window.google) {
    		google.maps.event.trigger(me.map, 'resize');
    		//Correct center after resize
    		me.setCenter(me.center);	
    	}
    	me.callParent(arguments);
    },
	setValue: Ext.emptyFn,

	//Functions for incremental diff
	setCenter: function (center) {
		var me = this;
		me.center = center;
		if(me.rendered && window.google) {
    		me.map.setCenter(new google.maps.LatLng(center[0],center[1]));	
		}
	},
	setMapType: function (mapType) {
		var me = this;
		me.mapType = mapType;
		if(me.rendered && window.google) {
			me.map.setMapTypeId(me.getMapType(mapType));
		}
	},
	setOptions: function (options) {
		var me = this;	
		me.options = options;
		if(me.rendered) {
			me.map.setOptions(me.getOptions());
		}
	},
	addMarker: function(index, def) {
     	var	me = this,
			map = this.map,
			marker, infoWindow, clickHandler, dragHandler,icon;

        if(me.rendered && window.google) {
            if(def.icon && def.icon[0] == "GoogleMapSimpleIcon" ) { 
    	        icon = "/icons/" + def.icon[1];	
            } else if(def.icon && def.icon[0] == "GoogleMapComplexIcon" ) { 
                icon = def.icon[1];
                icon = new google.maps.MarkerImage("/icons/" + icon.image
                        , new google.maps.Size(icon.size[0], icon.size[1])
		    		    , new google.maps.Point(icon.origin[0], icon.origin[1])
                        , new google.maps.Point(icon.anchor[0], icon.anchor[1])
                        );
            } else {
                icon = null;
            }

            marker = new google.maps.Marker({
                map : map,
                position : new google.maps.LatLng(def.position[0],def.position[1]),
                title : def.title,
                draggable : def.draggable,
                icon: icon
            });

            if(def.infoWindow) {
                infoWindow = new google.maps.InfoWindow({
                    content : def.infoWindow
                });
                clickHandler = function(map,marker,infoWindow) {
                    return function(e) {infoWindow.open(map,marker);};
                };
                google.maps.event.addListener(marker,'click',clickHandler(map,marker,infoWindow));
            } else {
                clickHandler = function(markerId) { return function(e) {
                    me.lastEditNo = itwc.global.controller.sendEditEvent(me.taskId,me.editorId,{index: markerId, event: "LEFTCLICK"});
                    };};
		        google.maps.event.addListener(marker,'click',clickHandler(index));
            }

            if(def.draggable) {
                dragHandler = function(markerId) { return function(e) {
                    me.lastEditNo = itwc.global.controller.sendEditEvent(me.taskId,me.editorId,{index: markerId, point : [e.latLng.lat(),e.latLng.lng()]});
                    };};

                google.maps.event.addListener(marker,'dragend', dragHandler(index));
            }
        }
        this.markers[index] = marker;
	},
	updateMarker: function(index, def) {
		var me = this,
			marker = me.markers[index],
			icon;
		
		if(me.rendered && window.google) {
			//Update position
			marker.setPosition(new google.maps.LatLng(def.position[0],def.position[1]));
			//Update icon
			if(def.icon && def.icon[0] == "GoogleMapSimpleIcon" ) { 
				icon = "/icons/" + def.icon[1];	
			} else if(def.icon && def.icon[0] == "GoogleMapComplexIcon" ) { 
				icon = def.icon[1];
				icon = new google.maps.MarkerImage("/icons/" + icon.image
						, new google.maps.Size(icon.size[0], icon.size[1])
						, new google.maps.Point(icon.origin[0], icon.origin[1])
						, new google.maps.Point(icon.anchor[0], icon.anchor[1])
						);
			} else {
				icon = null;
			}
			marker.setIcon(icon);
		}
	},
	removeMarker: function(index) {
        var me = this;
        if(me.rendered) {
            me.markers[index].setMap(null);
        }
        delete me.markers[index];
	},
	onDestroy: function() {
		if(this.map) {
            if(window.google) {
			    google.maps.event.clearInstanceListeners(this.map);
            }
			delete this.map;
		}
	}
});
