/*
* Managing components are containers that manage a set of child components.
* They can manage their own items as well as a set of windows. Windows are
* normally excluded from the "items" set of a container.
*/
Ext.define('itasks.mixin.Managing',{
	managing: true,
	
	initManaged: function() {
		this.managed = [];
	},
	managedAdd: function() {
		//If an index is given, we need to do some preparation
		if(Ext.isNumber(arguments[0])) {
			var index =	arguments[0];
			this.reserveManaged(index);
			arguments[0] = this.unmanagedIndex(index);
		}
		this.superclass.add.apply(this,arguments);
	},
	managedRemove: function(orig, autoDestroy) {
		
		var cmp = Ext.isNumber(orig) ? this.managed[orig] : orig;
		
		if(cmp.isXType('itasks_window')) {
			//Destroy the window
			this.unregisterManaged(cmp);
			cmp.destroy();
		} else {
			//Remove the right component
			this.unregisterManaged(cmp);
			this.superclass.remove.apply(this,[cmp,autoDestroy]);
		}	
	},
	reserveManaged: function(index) {
		//Create a placeholder at the right index
		this.managed.splice(index,0,null);
	},
	registerManaged: function(cmp) {	
		//Check for a placeholder in the set
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
	unregisterManaged: function(cmp) {
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