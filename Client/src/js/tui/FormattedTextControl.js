Ext.ns("itasks.tui");

itasks.tui.FormattedTextControl = Ext.extend(Ext.form.HtmlEditor,{
	initComponent: function() {
		Ext.apply(this, {
			width: 700,
			height: 300,
			// ids for selection marker elements
			selectionStartElId: this.id + '_marker-start',
			selectionEndElId: this.id + '_marker-end',
			acceptNewValue: true,
			suspendUpdate: false,
			mouseDown: false
		});
		itasks.tui.FormattedTextControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('update');
		var sendUpdate = new Ext.util.DelayedTask(function() {
			this.acceptNewValue = true;
			// suspend updates because events are triggered by this function's changes
			this.suspendUpdate = true;
			
			var doc = this.getDoc();
			var sel = Ext.isIE ? new DOMSelection(doc) : this.getWin().getSelection();
			var origRange = sel.getRangeAt(0);
			if (origRange.collapsed) {
				// no selection, insert start marker at cursor
				this.insertAtCursor("<markerstart id=\"" + this.selectionStartElId + "\"></markerstart>");
			} else {
				// create start/end markers
				var startEl = doc.createElement('markerstart');
				startEl.id = this.selectionStartElId;
				//startEl.setAttribute('class', 'marker-start');
				var endEl = doc.createElement('markerend');
				endEl.id = this.selectionEndElId;
				//endEl.setAttribute('class', 'marker-end');
				
				// get start/end point of current selection
				var startRange = origRange.cloneRange();
				var endRange = origRange.cloneRange();
				startRange.collapse(true);
				endRange.collapse(false);
				
				// insert markers & sync
				endRange.insertNode(endEl);
				startRange.insertNode(startEl);
				this.syncValue();
				
				// restore original selection
				sel.removeAllRanges();
				sel.addRange(origRange);
			}
			
			// send update & remove markers afterwards
			this.fireEvent('update');
			this.removeMarker(this.selectionStartElId);
			this.removeMarker(this.selectionEndElId);
			this.suspendUpdate = false;
		}, this);
		
		this.on('sync', function(editor) {
			if (!this.suspendUpdate && !this.mouseDown) {
				// ignore new values from server because content of editor has changed in meantime
				this.acceptNewValue = false;
				sendUpdate.delay(500);
			}
		});
	},
	
	initEditor: function() {
		itasks.tui.FormattedTextControl.superclass.initEditor.apply(this,arguments);
		
		doc = this.getDoc();
		// don't send update on mousedown to prevent problems with making selection using the mouse
		Ext.EventManager.addListener(doc, 'mousedown', function(){this.mouseDown = true;}, this);
		Ext.EventManager.addListener(doc, 'mouseup', function(){this.mouseDown = false; this.onEditorEvent();}, this);
		
		this.tb.setVisible(this.tb.items.length > 0); // hide toolbar without controls
		this.computeMarkers(); // compute markers in initial value
	},
	
	removeMarker: function(id) {
		var doc = this.getDoc();
		var markerNode = doc.getElementById(id);
		
		if(markerNode) {
			if(markerNode.firstChild) {
				// don't remove content of node
				markerNode.parentNode.replaceChild(markerNode.firstChild, markerNode);
			} else
				markerNode.parentNode.removeChild(markerNode);
		}
	},
	
	getValue: function() {
		return this.getRawValue();
	},
	
	setValue: function() {
		if (!this.acceptNewValue)
			return;
			
		itasks.tui.FormattedTextControl.superclass.setValue.apply(this,arguments);
		this.computeMarkers();
	},
	
	computeMarkers: function() {
		// search for selection markers
		var startNode = this.getDoc().getElementById(this.selectionStartElId);
		var endNode = this.getDoc().getElementById(this.selectionEndElId);
		if(startNode)
			if(endNode)
				this.setSelection(startNode, endNode);
			else
				// no end-marker, set cursor to start-marker
				this.setSelection(startNode, startNode);
		
		// remove markers to prevent problems with selection boundaries inside of them
		this.removeMarker(this.selectionStartElId);
		this.removeMarker(this.selectionEndElId);
	},
	
	setSelection: function(start, end) {
		// set selection from start to end element
		var doc = this.getDoc();
		var sel = Ext.isIE ? new DOMSelection(doc) : this.getWin().getSelection();
		sel.removeAllRanges();
		
		var range = Ext.isIE ? new DOMRange(doc) : doc.createRange();
		range.setStartAfter(start);
		range.setEndAfter(end);
		if (Ext.isIE)
			TextRangeUtils.convertFromDOMRange(range).select();
		else
			sel.addRange(range);
	}
});

Ext.reg("itasks.tui.FormattedText",itasks.tui.FormattedTextControl);