itasks.itwc_raw_empty = {
	cssCls: 'raw-empty',
	initDOMEl: function() {
		this.domEl.innerHTML = '(empty)';
	}
};
itasks.itwc_raw_editor = {
	cssCls: 'raw-editor'
};
itasks.itwc_raw_compoundeditor = {
	cssCls: 'raw-compound-editor'
};
itasks.itwc_raw_compoundcontent = {
	cssCls: 'raw-compound-content'
};
itasks.itwc_raw_parallel = {
	cssCls: 'raw-parallel',	
};
itasks.itwc_raw_step = {
	cssCls: 'raw-step',	
};
itasks.itwc_raw_interact = {
	cssCls: 'raw-interact'
};

itasks.itwc_raw_action = {
	cssCls: 'raw-action',	
	domTag: 'a',
	width: 'wrap',
	initDOMEl: function() {
		var me = this, el = me.domEl;

		el.innerHTML = me.action;
		el.href = '#';
		el.classList.add(this.cssPrefix + (me.enabled ? 'raw-action-enabled' : 'raw-action-disabled'));
		el.addEventListener('click',function(e) {
			me.doActionEvent(me.taskId,me.action);
			e.preventDefault();
		});
    },
	onAttributeChange: function(name,value) {
		var me = this, el = me.domEl;
		switch(name) {
			case 'enabled':
				me.enabled = value;
				if(me.enabled) {
					el.classList.remove(this.cssPrefix + 'raw-action-disabled');
					el.classList.add(this.cssPrefix + 'raw-action-enabled');
				} else {
					el.classList.remove(this.cssPrefix +'raw-action-enabled');
					el.classList.add(this.cssPrefix +'raw-action-disabled');
				}
				break;
		}
	}
};
itasks.itwc_raw_form = {
	cssCls: 'raw-form',	
};
itasks.itwc_raw_form_item = {
	cssCls: 'raw-form-item',	
};
