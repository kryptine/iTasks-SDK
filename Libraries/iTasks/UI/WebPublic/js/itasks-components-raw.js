itasks.Raw = class extends itasks.Component {
	initDOMEl() {
		var me = this;
		me.headerEl = document.createElement('div');
		me.headerEl.classList.add(me.cssPrefix + 'header');
		me.headerEl.innerHTML = me.type + ": " + JSON.stringify(me.attributes);

		me.domEl.appendChild(me.headerEl);
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);
	}
	onAttributeChange(name,value) {
		me.headerEl.innerHTML = me.type + ": " + JSON.stringify(me.attributes);
	}
};
itasks.RawRecord = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-record';
	}
};
itasks.RawCons = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-cons';
	}
};
itasks.RawVarCons = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-var-cons';
	}
};
itasks.RawInteract = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-interact';
	}
};
itasks.RawStep = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-step';
	}
};
itasks.RawParallel = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-parallel';
	}
};
itasks.RawEmpty = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-empty';
	}
	initDOMEl() {
		this.domEl.innerHTML = '(empty)';
	}
};
itasks.RawAction = class extends itasks.Raw {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'raw-action';
		this.domTag = 'a';
		this.attributes = {
			width: 'wrap'
		};
	}
	initDOMEl() {
		var me = this, el = me.domEl;

		el.innerHTML = me.attributes.actionId;
		el.href = '#';
		el.classList.add(this.cssPrefix + (me.attributes.enabled ? 'raw-action-enabled' : 'raw-action-disabled'));
		el.addEventListener('click',function(e) {
			me.doEditEvent(me.attributes.taskId,null,me.attributes.actionId);
			e.preventDefault();
		});
	}
	onAttributeChange(name,value) {
		var me = this, el = me.domEl;
		switch(name) {
			case 'enabled':
				if(value) {
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
