Ext.ns('itasks.ttc.common');

itasks.ttc.TTC_FADE_DURATION = .75;

itasks.ttc.common.attachDocumentLinkInformation = function() {
	
	var links   = Ext.query("a[name=x-form-document-link]");
	var plinks = Ext.query("a[name=x-form-document-preview-link]");
	
	for(var x=0; x < links.length; x++){
		var link = links[x];
		
		link.href = Ext.urlAppend(link.href,'session='+itasks.app.session);
		link.name = "";
		
		for(var y=0; y < plinks.length; y++){				
			if(plinks[y].id == link.id){
				var plink = plinks[y];		

				plink.href="javascript:itasks.util.preview('"+link.href.replace( 'download','preview')+"')";
				plink.name = "";
			}
		}
	}
};

itasks.ttc.common.setupHotkeys = function(tb, parent) {
	// collect all hotkeys
	var hotkeys = new Ext.util.MixedCollection();
	
	var collectHotkeys = function(item) {
		var hk = item.hotkey;
		if(hk) {
			hotkeys.add({hotkey: hk, menuitem: item});
			// append hotkey-string to item name
			var str = hk.key;
			if (hk.shift)	str = "Shift+" + str;
			if (hk.alt)		str = "Alt+" + str;
			if (hk.ctrl)	str = "Ctrl+" + str;
			item.setText(item.text + "&nbsp;&nbsp;&nbsp;&nbsp;(" + str + ")");
		} else if (item.items || item.menu) {
			var children =  item.items || item.menu.items;
			for(var i = 0; i < children.length; i++) {
				collectHotkeys(children.get(i));
			}
		}
	}
	
	collectHotkeys(tb);
	
	// disabled old hotkeys
	if (parent.keyMap)
		parent.keyMap.disable();

	if (hotkeys.getCount() == 0)
		return;
	
	// build up config array for KeyMap
	var conf = new Array();
	hotkeys.each(function(hk) {
		var h = hk.hotkey;
		conf[conf.length] = {
			key: h.key,
			ctrl: h.ctrl,
			alt: h.alt,
			shift: h.shift,
			stopEvent: true,
			handler: function() {
				var item = hk.menuitem;
				// only fire hotkey-event if item is not disabled
				if (!item.disabled) {
					parent.addUpdate(item.name, item.value);
					parent.sendUpdates();
				}
			}
		};
	});

	parent.keyMap = new Ext.KeyMap(parent.getEl(), conf);
};
