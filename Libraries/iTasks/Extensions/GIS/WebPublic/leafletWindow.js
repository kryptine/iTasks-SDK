L.Window = L.Control.extend({
    options: {
        position: 'topleft'
    },
    setInitPos: function(pos) {
        this._initPos = pos;
    },
    setTitle: function(title) {
        this._title = title;
    },
    setContent: function(content) {
        this._content = content;
    },
    addRelatedMarker: function(marker) {
        if (!this._relatedMarkers) this._relatedMarkers = new Set();

        this._relatedMarkers.add(marker);
    },
    onAdd: function (map) {
        this._map = map;
        var prefix = 'leaflet-popup';
        var container = this._container = L.DomUtil.create('div', prefix + '-content-wrapper');
        var titleBar = L.DomUtil.create('div', '', container);
        titleBar.style = "height: 20px; background: -webkit-linear-gradient(top, #ffa554, #fb7322); border-radius: 12px; padding: 6px; overflow: hidden; white-space: nowrap; cursor: default;";
        titleBar.innerHTML = this._title;
        this._contentNode = L.DomUtil.create('div', '', container);
        this._contentNode.style = "padding: 10px;"
        this._contentNode.innerHTML = this._content;

        // absolute -> otherwise windows influence each other if multiple are present
        container.style = "margin: 0px; position: absolute;";
        this._setPos(this._initPos);

        L.DomEvent.disableClickPropagation(container);
        L.DomEvent.disableScrollPropagation(container);
        L.DomEvent.on(container, 'contextmenu', L.DomEvent.stopPropagation);
        this.dragging = false;
        L.DomEvent.on(titleBar, 'mousedown', function(e) {
                                                 // store delta between left upper corner of window and mouse position
                                                 const containerRect = this._container.getBoundingClientRect();
                                                 this.dragging = [e.clientX - containerRect.left, e.clientY - containerRect.top];
                                                 L.DomUtil.disableTextSelection();
                                                 this._container.style.opacity = 0.6;
                                                 L.DomUtil.toFront(container);
                                             },
                      this);
        L.DomEvent.on(document, 'mouseup', this._mouseUp, this);
        L.DomEvent.on(document, 'mousemove', this._mouseMove, this);

        var closeButton = this._closeButton = L.DomUtil.create('a', prefix + '-close-button', titleBar);
        closeButton.href = '#close';
        closeButton.innerHTML = '&#215;';
        closeButton.style = 'padding: 0px 6px 0px 0px; top: auto;';

        L.DomEvent.on(closeButton, 'mouseup', this._onCloseButtonClick, this);

        this._contentWidth  = this._contentNode.offsetWidth;
        this._contentHeight = this._contentNode.offsetHeight;

        this._relatedMarkerConnectors = {};
        // handle related markers already added
        if (this._relatedMarkers) {
            map.eachLayer((l) => {
                if (this._relatedMarkers.has(l.markerId)) {
                    this._addRelatedMarker(l);
                }
            });
        }
        // handle related markers added in the future
        map.on('layeradd', this._onLayerAdd, this);
        // handle related markers removed in the future
        map.on('layerremove', this._onLayerRemove, this);
        // update related marker connectors positions on zoom/move
        map.on('zoom', this._updateRelatedMarkerConnectorPositions, this);
        map.on('move', this._updateRelatedMarkerConnectorPositions, this);

        return container;
    },
    _onLayerAdd: function(e) {
        const marker = e.layer;
        if (this._relatedMarkers.has(marker.markerId)) {
            this._addRelatedMarker(marker);
        }
    },
    _onLayerRemove: function(e) {
        const markerId = e.layer.markerId;
        if (this._relatedMarkers.has(markerId)) {
            this._relatedMarkerConnectors[markerId].polyline.remove();
            delete this._relatedMarkerConnectors[markerId];
        }
    },
    _addRelatedMarker: function(marker) {
        const connector = {polyline: L.polyline([], {className: ''}), position: marker.getLatLng()};
        this._relatedMarkerConnectors[marker.markerId] = connector;
        connector.polyline.addTo(this._map);
        this._updateRelatedMarkerConnectorPosition(connector);
    },
    _updateRelatedMarkerConnectorPositions: function() {
        Object.values(this._relatedMarkerConnectors).forEach(this._updateRelatedMarkerConnectorPosition, this);
    },
    _updateRelatedMarkerConnectorPosition: function(connector) {
        const windowCentrePos = { x: this._position.x + this._contentNode.offsetWidth/2
                                , y: this._position.y + this._contentNode.offsetHeight/2 };
        connector.polyline.setLatLngs([this._map.containerPointToLatLng(windowCentrePos), connector.position]);
    },
    _mouseUp: function(e) {
        this.dragging = false;
        L.DomUtil.enableTextSelection();
        this._container.style.opacity = 1.0;
    },
    _mouseMove: function(e) {
        var dragging = this.dragging;
        if (dragging) {
            const mapPos = this._map.mouseEventToContainerPoint(e);
            // delta (stored in 'dragging') to compensate for where inside title bar drag was started
            this._setPos({x: mapPos.x - dragging[0], y: mapPos.y - dragging[1]});
            this._updateRelatedMarkerConnectorPositions();
        }
    },
    _onCloseButtonClick: function () {
        this._onWindowClose(); // injected by Clean code (sends remove event to server)
        this.remove();
    },
    _setPos: function(p) {
        this._position = p;
        this._container.style.transform = "translate(" + p.x + "px, " + p.y + "px)";
    },
    onRemove: function (map) {
        Object.values(this._relatedMarkerConnectors).forEach((m) => m.polyline.remove());
        L.DomEvent.off(document, 'mouseup', this._mouseUp, this);
        L.DomEvent.off(document, 'mousemove', this._mouseMove, this);
        map.off('layeradd', this._onLayerAdd, this);
        map.off('layerremove', this._onLayerRemove, this);
        map.off('zoom', this._updateRelatedMarkerConnectorPositions, this);
        map.off('move', this._updateRelatedMarkerConnectorPositions, this);
    }
});
L.window = function (options, source) {
    return new L.Window(options, source);
};
