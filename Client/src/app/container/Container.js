Ext.define('itasks.container.Container',{
	extend: 'Ext.container.Container',
	alias: 'widget.itasks_container',
	requires: ['itasks.layout.VHBox'],

	initComponent: function() {
		Ext.applyIf(this,{
			layout: {type: 'vhbox', direction: this.direction, valign: this.valign, halign: this.halign}
		});
		
		//If no specification of the container width or height is given, make it wrapping
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hwrap = true;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}
		if(!this.height) {
			this.height = 10000;
			this.simulatedHeight = true;
		}
		if(!this.width) {
			this.width = 10000;
			this.simulatedWidth = true;
		}
		this.initManaged();
		this.callParent(arguments);	
	},
	add: function() {
		//If an index is given, we need to do some preparation
		if(Ext.isNumber(arguments[0])) {
			var index =	arguments[0];
			this.reserveManaged(index);
			arguments[0] = this.unmanagedIndex(index);
		}
		this.callParent(arguments);
	},
	remove: function(comp,autoDestroy) {
		var cmp = Ext.isNumber(comp) ? this.managed[comp] : comp;
	
		if(cmp.isXType('itasks_window')) {
			this.removeManaged(cmp);
			cmp.destroy();
		} else {
			//Remove the right component
			this.removeManaged(cmp);
			this.callParent([cmp,autoDestroy]);
		}
	},
	onAdd: function(cmp,index) {
		this.addManaged(cmp);
		this.callParent(arguments);
	},
	initManaged: function() {
		//Create the managed items array
		//This is necessary because extjs keeps windows out of
		//the collection of child items of a container while itasks
		//considers them as yet another child item.
		
		//All 'normal' items are added by containers and
		//windows add themselves.
		
		//The add & remove methods of a container are overerwritten 
		//to correct indices to account for windows
		this.managed = [];	
	},
	reserveManaged: function(index) {
		//Create a placeholder at the right index
		this.managed.splice(index,0,null);
	},
	addManaged: function(cmp) {	
		//Check for an 'empty slot' in the set
		//If it is not found, add it to the end of the set
		var i, len = this.managed.length;
		for(i = 0; i < len; i++) {
			if(this.managed[i] === null) {
				this.managed[i] = cmp;
				return;
			}
		}
		this.managed.push(cmp);
	},
	removeManaged: function(cmp) {
		var i, len = this.managed.length;
		
		if(Ext.isNumber(cmp)) {
			this.managed.splice(cmp,1);
		} else {
			for(i = 0; i < len; i++) {
				if(this.managed[i] == cmp) {
					this.managed.splice(i,1);
					return;
				}
			}
		}
	},
	unmanagedIndex: function(index) {
		var i, unmanaged = 0;
		for(i = 0; i < index; i++) {
			if(!this.managed[i].isXType('itasks_window')) {
				unmanaged++;
			}
		}
		return unmanaged;
	}
});
