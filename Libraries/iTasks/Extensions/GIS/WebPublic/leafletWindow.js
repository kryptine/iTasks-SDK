L.Window = L.Control.extend({
     options: {
        position: 'topleft'
    },
    setLatLng: function(pos) {
        this._initPos = pos;
    },
    setTitle: function(title) {
        this._title = title;
    },
    setContent: function(content) {
        this._content = content;
    },
    onAdd: function (map) {
        var prefix = 'leaflet-popup';
        var container = this._container = L.DomUtil.create('div', prefix + '-content-wrapper');
        var titleBar = L.DomUtil.create('div', '', container);
        titleBar.style = "height: 20px; background: -webkit-linear-gradient(top, #ffa554, #fb7322); border-radius: 12px; padding: 6px; overflow: hidden; white-space: nowrap; cursor: default;";
        titleBar.innerHTML = this._title;
        this._contentNode = L.DomUtil.create('div', '', container);
        this._contentNode.style = "padding: 10px;"
        this._contentNode.innerHTML = this._content;

        const mapPt = this._map.latLngToContainerPoint(this._initPos);
        // absolute -> otherwise windows influence each other if multiple are present
        container.style = "margin: 0px; position: absolute;";
        this._setPos(mapPt);

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

        return container;
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
        L.DomEvent.off(document, 'mouseup', this._mouseUp, this);
        L.DomEvent.off(document, 'mousemove', this._mouseMove, this);
    }
});
L.window = function (options, source) {
    return new L.Window(options, source);
};
