itasks.itwc_view_string = {
	container: false,
	initDOMEl: function() {
		this.domEl.innerHTML = this.value || '';
	},
	setValue: function(html) {
		this.domEl.innerHTML = html;	
	},
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
};
itasks.itwc_view_html = {
	initDOMEl: function() {
		this.domEl.innerHTML = this.value || '';
	},
	setValue: function(html) {
		this.domEl.innerHTML = html;	
	},
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
};
itasks.itwc_view_document = {
    initDOMEl: function() {
        var me = this;
        if(me.value) {
            me.setValue(me.value);
        }
    },
    setValue: function(value) {
        var me = this;
        if(value) {
            me.domEl.innerHTML = '<a href="'+value.contentUrl+'" target="_blank">'+value.name+'</a>';
        }
    }
};
itasks.itwc_view_checkbox = {
	domTag: 'input',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;

		el.type = 'checkbox';
		el.checked = me.value;
		el.disabled = true;
	},
	onAttributeChange(name,value) {
		switch(name) { 
			case 'value': this.domEl.checked = value; break;
		}
	}
};
itasks.itwc_view_slider = {
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        el.min = me.minValue;
        el.max = me.maxValue;
        el.value = me.value;
        el.disabled = true;
    },
	onAttributeChange(name,value) {
		switch(name) { 
			case 'value': this.domEl.value = value; break;
		}
	}
};
itasks.itwc_view_progress = {
    domTag: 'progress',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.innerHTML = me.text;
        el.max = 100;
        if(typeof me.value == 'number') {
            el.value = me.value * 100;
        }
    },
    setValue: function(value) {
        this.domEl.value = value * 100;
    }
};
