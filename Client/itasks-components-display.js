itasks.TextView = {
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
itasks.HtmlView = {
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
itasks.ProgressBar = {
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
