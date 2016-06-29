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
	width: 'flex',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.innerHTML = me.text;
		el.min = 0;
        el.max = 100;
        if(typeof me.value == 'number') {
            el.value = me.value;
        }
    },
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': this.domEl.value = value; break;
			case 'text': this.domEl.innerHTML = value; break;
		}
	}
};
