Ext.define('itasks.container.TabContainer',{
	extend: 'Ext.tab.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks_tab_container',
	initComponent: function() {
		
		//If no specification of the container width or height is given, make it flexible
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hflex = 1;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vflex = 1;
		}
		
		//This component uses a more complex suspend events mechanism
		//that checks the resumeEvents method does not take into account
		//when suspendEvents has been called multiple times 
		this.suspended = 0;
		
		this.activeTab = this.active;
		
		this.addEvents('edit');
		this.enableBubble('edit');
		this.addManagedListener(this,'tabchange',this.onTab,this);
		
		this.callParent(arguments);
	},
	setActiveTab: function(idx) {
		if(Ext.isNumber(idx)) {
			this.active = idx;
		}
		this.callParent(arguments);
	},
	onTab: function(cnt,tab) {
		if(cnt.taskId) {
			this.fireEvent('edit',cnt.taskId,'top',tab.index);
		}
	},
	insert: function(idx,tab) {
		if((this.suspended++) == 0) {this.suspendEvents();}

		this.callParent(arguments);
		
		if(idx == this.active) {
			this.setActiveTab(idx);
		}
		if((--this.suspended) == 0) {this.resumeEvents();}
	},
	remove: function(idx) {
		if((this.suspended++) == 0) {this.suspendEvents();}
		
		this.callParent(arguments);
		
		if((--this.suspended) == 0) {this.resumeEvents();}
	},
	replace: function(idx, tab) {
		if((this.suspended++) == 0) {this.suspendEvents();}
		
		this.remove(idx);
		this.insert(idx,tab);
		
		if((--this.suspended) == 0) {this.resumeEvents();}
	}
});
