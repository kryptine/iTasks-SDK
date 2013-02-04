/**
* Mixin for adding hotkey handler to containers.
*/
Ext.define('itwc.container.HotkeyArea',{

	mixins: ['itwc.component.edit.Editable'],
	hotkeys: null,

	initHotkeys: function() {
		this.setHotkeys(this.hotkeys);	
	},
	setHotkeys: function(hotkeys) {
		var me = this,
			binding = [],
			key, i;

		//Destroy old key map if it exists
		if(me.hotkeys && me.hotkeys.destroy) {
			me.hotkeys.destroy(false);
		}
		//Create the new keymap 
		if(hotkeys && hotkeys.length) {
			for(i = 0; i < hotkeys.length; i++) {
				key = hotkeys[i][0];
				key.fn = function(taskId,actionId) {
					return function() {me.onHotkey(taskId,actionId);};
				}(hotkeys[i][1].taskId, hotkeys[i][1].actionId);
				key.defaultEventAction = 'stopEvent';

				binding.push(key);
			}
			me.hotkeys = new Ext.util.KeyMap({
				target: me.getEl(),
				binding: binding,
				scope: me
			});
		} else {
			me.hotkeys = null;
		}
	},
	onHotkey: function(taskId,actionId) {
		var me = this;

		me.viewport = me.findViewport();
		me.viewport.fireEvent('action',taskId, actionId);
	},
	destroyHotkeys: function() {
		var me = this;
		if(Ext.isObject(me.hotkeys)) {
			me.hotkeys.destroy(false);
		}
	}
});
