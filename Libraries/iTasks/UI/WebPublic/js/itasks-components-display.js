itasks.TextView = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'textview';
		this.container = false;
	}
	initDOMEl() {
		this.domEl.innerHTML = this.attributes.value || '';
	}
	setValue(html) {
		this.domEl.innerHTML = html;	
	}
	onAttributeChange(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
}
itasks.HtmlView = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'htmlview';
	}
	initDOMEl() {
		this.domEl.innerHTML = this.attributes.value || '';
	}
	setValue(html) {
		this.domEl.innerHTML = html;	
	}
	onAttributeChange(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
}
itasks.ProgressBar = class extends itasks.Component {
	constructor(spec) {
		this.domTag = 'div';
		this.cssCls = 'progress';
	}
	initDOMEl() {
		var me = this,
			el = this.domEl;

		//https://stackoverflow.com/questions/41429906/how-to-display-data-label-inside-html5-progress-bar-cross-browser-compatible
		var child = document.createElement('span');
		child.setAttribute('class', 'value');
		el.appendChild(child);

		me.setProgress(me.attributes.value);
		me.setText(me.attributes.text);
	}
	setProgress(value) {
		this.domEl.children[0].style=typeof value == 'number' ? ('width:'+value+'%;') : '';
	}
	setText(text) {
		this.domEl.setAttribute('title', text);
	}
	onAttributeChange(name,value) {
		switch(name) {
			case 'value':
				this.setProgress(value);
				break;
			case 'text':
				this.setText(value);
				break;
		}
	}
};
