itasks.itwc_raw_empty = {
	cssCls: 'raw-empty',
	initDOMEl: function() {
		this.domEl.innerHTML = '(empty)';
	}
};
itasks.itwc_raw_record = {
	cssCls: 'raw-record'
};
itasks.itwc_raw_cons = {
	cssCls: 'raw-cons'
};
itasks.itwc_raw_var_cons = {
	cssCls: 'raw-var-cons'
};
itasks.itwc_raw_interact = {
	cssCls: 'raw-interact'
};
itasks.itwc_raw_step = {
	cssCls: 'raw-step',	
};
itasks.itwc_raw_parallel = {
	cssCls: 'raw-parallel',	
};

itasks.itwc_raw_action = {
	cssCls: 'raw-action',	
	domTag: 'a',
	width: 'wrap',
	initDOMEl: function() {
		var me = this, el = me.domEl;

		el.innerHTML = me.actionId;
		el.href = '#';
		el.classList.add(this.cssPrefix + (me.enabled ? 'raw-action-enabled' : 'raw-action-disabled'));
		el.addEventListener('click',function(e) {
			me.doActionEvent(me.taskId,me.actionId);
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
