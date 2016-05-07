//Test script for new itasks embedding style
itasks = {};

//Core behavior
itasks.Component = {

	domTag: 'div',
	domEl: null,

	container: true,
	containerEl: null,

	cssPrefix: 'itasks-',
	cssCls: 'component',

	width: 'flex',
	height: 'flex',
	direction: 'vertical',

	parentCmp: null,
	children: [],

	initialized: false,

	init: function() {
		var me = this;
		me.initSaplCustomization();
		me.initComponent();
		me.initChildren();
		me.renderComponent();

		me.initialized = true;
		return me;
	},
	initSaplCustomization: function() { //When necessary, apply customizatons for Check if some of the component's methods are custom defined using sapl/js
		var me = this, fun, evalfun;
		//Initialize linked sapl functions 
		if(me.saplDeps != null && me.saplDeps != '') {
			me.saplDeps = me.evalJs(me.saplDeps);
        }
		//Decode and evaluate the sapl initialization function
		if(me.saplInit !=null && me.saplInit!= '') {
			Sapl.feval([me.evalJsVal(me.saplInit),[___wrapJS(me),"JSWorld"]]);
		}
	},
	initComponent: function() {}, //Abstract method: every component implements this differently
	initChildren: function() {
		var me = this;
		me.children.forEach(function(spec,i) {
			me.beforeChildInsert(i,spec);
			me.children[i] = me.createChild(spec);
			me.children[i].init();
			me.afterChildInsert(i);
		});
	},
	renderComponent: function() {
		var me = this;
		if(me.domEl === null) { //Create a new dom element
        	me.domEl = document.createElement(me.domTag);
		} else { //Clear an existing element
			me.domEl.innerHTML = null;
		}
		//Initialially make the outer dom element also the container element that holds the child components
		me.containerEl = me.domEl;
			
		//Style the dom element
		me.domEl.classList.add(me.cssPrefix + me.cssCls);

		//Custom initialization after the dom element has been rendered
		me.initDOMEl();
		//Size the element
		me.initDOMElSize();
		//Set margins and alignment
		me.initDOMElMargins();

		if(me.container) {
			me.initContainerEl();
		}
		//Add the the child renderings 
		me.children.forEach(function(child) {
			me.containerEl.appendChild(child.domEl);
		});
	},
	initDOMEl: function() {},

	initDOMElSize: function() {
		var me = this,
			el = me.domEl,
			width = me.width,
			height = me.height,
			direction = (me.parentCmp && me.parentCmp.direction) || 'vertical';

		//Set width
		if(width === 'flex') {
			if(direction == 'horizontal') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (width === 'wrap') {
			if(direction == 'horizontal') {
				el.classList.add(me.cssPrefix + 'wrapping-horizontal');
            }
        } else {
			el.style.width = width + 'px';
			el.style.minWidth = width + 'px';
        }
		//Set height
		if(height === 'flex') {
			if(direction == 'vertical') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (height === 'wrap') {
			if(direction == 'vertical') {
				el.classList.add(me.cssPrefix + 'wrapping-vertical');
			}
		} else {
			el.style.height = height + 'px';
			el.style.minHeight = height + 'px';
		}
    },
	initDOMElMargins: function() {
		var me = this,
			el = me.domEl,
            width = me.width,
            height = me.height,
			margins = (me.margins && me.margins.split(' ')) || ['0','0','0','0'];

		if(!me.parentCmp) { //Do not set margins on the root component, let the embedding page handle that
			return;
		}
		var parentDirection = (me.parentCmp && me.parentCmp.direction) || 'vertical',
            parentVAlign = (me.parentCmp && me.parentCmp.valign) || 'top',
            parentHAlign = (me.parentCmp && me.parentCmp.halign) || 'left',
			curIdx = me.parentCmp.findChild(me),
			lastIdx = me.parentCmp.children.length - 1;

        //Set margins as specified
		el.style.margin = margins.map(function(s) { return s + 'px'}).join(' ');

		//Set margins to auto based on alignment of parent
        if(parentDirection == 'vertical') {
			if(width !== 'flex') {
				switch(parentHAlign) {
					case 'left': el.style.marginRight = 'auto'; break;
					case 'center': el.style.marginRight = 'auto'; el.style.marginLeft = 'auto'; break;
					case 'right': el.style.marginLeft = 'auto'; break;
				}
			}
            //If this element is the first, maybe also adjust top margin;
            if(curIdx === 0 && (parentVAlign == 'middle' || parentVAlign == 'bottom')) {
				el.style.marginTop = 'auto';
			}
			//If this element is the last, maybe also adjust bottom margin;
			if(curIdx === lastIdx && (parentVAlign == 'middle' || 'top')) {
				el.style.marginBottom = 'auto';
			}
		} else {
			if(height !== 'flex') {
				switch(parentVAlign) {
					case 'top': el.style.marginBottom = 'auto'; break;
					case 'middle': el.style.marginBottom = 'auto'; el.style.marginTop = 'auto'; break;
					case 'bottom': el.style.marginTop = 'auto'; break;
				}
			}
			//If this element is the first, maybe also adjust left margin;
			if(curIdx === 0 && (parentHAlign == 'center' || parentHAlign == 'right')) {
                el.style.marginLeft = 'auto';
            }
            //If this element is the last, maybe also adjust right margin;
            if(curIdx === lastIdx && (parentHAlign == 'center' || parentHAlign == 'left')) {
                el.style.marginRight = 'auto';
			}
		}
	},
	initContainerEl: function() {
		var me = this,
            el = me.containerEl,
            horizontal = (me.direction && (me.direction === 'horizontal')) || false;

        el.classList.add(me.cssPrefix + (horizontal ? 'hcontainer' : 'vcontainer'));

		if(me.padding) {
            el.style.padding = me.padding.split(' ').map(function(s) { return s + 'px'}).join(' ');
        }
	},
	doEditEvent: function (taskId, editorId, value) {
		if(this.parentCmp) {
			this.parentCmp.doEditEvent(taskId, editorId, value);
		}
	},
	doActionEvent: function(taskId, actionId) {
		if(this.parentCmp) {
			this.parentCmp.doActionEvent(taskId, actionId);
		}
	},
	findChild: function(obj) {
		var me = this, num = me.children.length, i;

		for( i = 0; i < num; i++) {
			if(me.children[i] === obj) {	
				return i;
			}
		}
	},
	createChild: function(spec) {
		var me = this,
			type = spec.xtype || 'Component';
		if(itasks[type]) {
			return Object.assign(Object.create(itasks.Component),itasks[type],{parentCmp:me,children:[]},spec);
		} else {
			return Object.assign(Object.create(itasks.Component),{parentCmp:me,children:[]},spec);
		}
	},
	insertChild: function(idx = 0, spec = {}) {
		var me = this,
			child = null,
			isLast = (idx == me.children.length);
		
		me.beforeChildInsert(idx,spec);

		//Create the child object
		child = me.initialized ? me.createChild(spec) : spec ;

		//Add the child to the collection of children
		me.children.splice(idx,0,child);

		if(me.initialized) {
			//Initialize, if we are already initialized
			child.init();
			//Add the child to the dom
			if(isLast) {
				me.containerEl.appendChild(child.domEl);
			} else {
				me.containerEl.insertBefore(child.domEl,me.containerEl.childNodes[idx]);
			}
		} 
		me.afterChildInsert(idx);
	},
	beforeChildInsert: function(idx,spec) {},
	afterChildInsert: function(idx) {},
	removeChild: function(idx = 0) {
		var me = this;

		me.beforeChildRemove(idx);

		if(me.initialized) {
			me.containerEl.removeChild(me.containerEl.childNodes[idx]);
		}
		me.children.splice(idx,1);	
	},
	beforeChildRemove: function(idx) {},
	setAttribute: function(name,value) {
		var me = this;
	
		me[name] = value;	
		me.onAttributeChange(name,value);
	},
	onAttributeChange: function(name,value) {},
	onUIChange: function(change) {
		var me = this;
		if(change) {
			switch(change.type) {
				case 'replace':
					me.onReplaceUI(change.definition);
					break;
				case 'change':
					me.onChangeUI(change.attributes,change.children);
					break;
			}
		}
	},
	onReplaceUI: function(spec) {
		var me = this, idx;

		if(me.parentCmp) {
			idx = me.parentCmp.findChild(me);
			if(idx >= 0 ) {
				me.parentCmp.removeChild(idx);
				me.parentCmp.insertChild(idx,spec);
			}
		}
	},
	onChangeUI: function(attributeChanges,childChanges) {
		var me = this, idx;

		//Handle attribute changes
		if(attributeChanges instanceof Array) {
			attributeChanges.forEach(function(change) {
				me.setAttribute(change.name,change.value);
			});
		}
		//Handle child changes
		childChanges.forEach(function(change) {
			idx = change[0];
			switch(change[1]) {
				case 'change':
					if(idx >= 0 && idx < me.children.length) {
						me.children[idx].onUIChange(change[2]);
					} else {
						console.log("UNKNOWN CHILD",idx,me.children.length,change);
					}
					break;
				case 'insert':
					me.insertChild(idx,change[2]);
					break;
				case 'remove':
					me.removeChild(idx);
					break;
			}
		});
	},
	onShow: function() {
		this.children.forEach(function(child) { child.onShow(); });
	},
	onHide: function() {
		this.children.forEach(function(child) { child.onHide(); });
	},
	/* Utility methods */
	evalJs: function(js) {
		var h = document.getElementsByTagName("head")[0],
			s = document.createElement("script");
		s.type = "text/javascript";
		s.appendChild(document.createTextNode(js));
		h.appendChild(s);
		h.removeChild(s);

		//Make sure that the dynamics unification is specialized for javavascript functions
		if(typeof ___SystemDynamic__unify === "function" && ___SystemDynamic__unify != _gen_unify){
			_orig_unify_fun = ___SystemDynamic__unify;
			___SystemDynamic__unify = _gen_unify;
		}
		return null;
	},
	evalJsVal: function(js) {
		var out;
 		eval("out = " + js + ";");
        return out;
	}
};
itasks.Loader = {
	cssCls: 'loader',

	initDOMEl: function() {
		this.domEl.innerHTML = "Loading...";
	}
};
itasks.Viewport = {
	cssCls: 'viewport',

	initComponent:function() {
		var me = this;	

		//Use the page url as default taskUrl
		if(!me.taskUrl) {	
			me.taskUrl = window.location;
		}
		//Create a temporary root element
		me.insertChild(0,{xtype:'Loader', parentCmp: me});

		//Register the viewport with the iTasks service
		me.service = itasks.Service.getInstance();
		me.service.register(me);
	},
	getParentViewport: function() {
		var me = this, parentVp = me.parentCmp;
		while(parentVp) {
			if(parentVp.cssCls == 'viewport') { //Bit of a hack...
				return parentVp;
			}
			parentVp = parentVp.parentCmp;
		}
		return null;
	},
	doEditEvent: function (taskId, editorId, value) {
		var me = this;
		me.service.doEditEvent(taskId, editorId, value);
	},
	doActionEvent: function(taskId, actionId) {
		var me = this;
		me.service.doActionEvent(taskId, actionId);
	},
	onInstanceUIChange: function(change) {
		this.children[0].onUIChange(change);
	}
};

//Convenience function for concisely creating viewports
itasks.viewport = function(spec,domEl) {
	return Object.assign(Object.create(itasks.Component), itasks.Viewport, spec, {domEl:domEl}).init();
};


//Web service proxy/multiplexer class
//This is is a singleton because all itask component objects need to share their
//connections with the server in order to limit the number of connections.

itasks.Service = {
	instances: {},
	register: function(viewport) {
		var me = this,
			taskUrl, parentViewport, taskInstance, connection;

		if(taskInstance = viewport.instanceNo) {
			//Connect to an existing task instance
			me.registerInstance_(viewport);	
			
		} else if(taskUrl = viewport.taskUrl) {
			//Create a new task instance
			me.createTaskInstance_(taskUrl, function(instanceNo,instanceKey) {
			
				//Store the instanceNo and key on the viewport
				viewport.instanceNo = instanceNo;
				viewport.instanceKey = instanceKey;

				me.registerInstance_(viewport);	
			});
		}
	},
	registerInstance_: function(viewport) {
		var me = this, parentViewport, connection, instanceNo = viewport.instanceNo;
		
		//If the viewport is embedded in another viewport, reuse its connection
		if(parentViewport = viewport.getParentViewport()) {
			connection = me.instances[parentViewport.instanceNo].connection;
			connection.addTaskInstance(instanceNo, viewport.onInstanceUIChange.bind(viewport));
		} else {
			//Create the connection
			connection = Object.assign(Object.create(itasks.Connection),{taskUrl:viewport.taskUrl});
			connection.addTaskInstance(instanceNo, viewport.onInstanceUIChange.bind(viewport)); 
			connection.connect();
		}	
		//Send reset event...
		connection.sendEvent(instanceNo,{instanceNo: instanceNo, resetEvent: instanceNo});
		//Register the instance
		me.instances[instanceNo] = {connection: connection, viewport: viewport}
	},
	doEditEvent: function(taskId, editorId, value) {
		var me = this,
			instanceNo = taskId.split("-")[0];

		me.instances[instanceNo].connection.sendEvent(instanceNo,
			{instanceNo:instanceNo, editEvent: JSON.stringify([taskId,editorId,value])});
	},
	doActionEvent: function(taskId, actionId) {
		var me = this,
			instanceNo = taskId.split("-")[0];
	
		me.instances[instanceNo].connection.sendEvent(instanceNo,
			{instanceNo:instanceNo, actionEvent: JSON.stringify([taskId,actionId])});
	},
	unregister: function(viewport) {
	},
	createTaskInstance_: function(taskUrl, callback) {
		var me = this, xhr;
		//Send request
		xhr = new XMLHttpRequest();
		xhr.open('GET', taskUrl + '/new', true);
		xhr.onload = function(e) {
			var msg = JSON.parse(e.target.responseText);
			callback.bind(me)(msg['instanceNo'],msg['instanceKey']);
		};
		xhr.send();	
	},
	getInstance: function() {
		if(typeof itasks.Service.INSTANCE === 'undefined') {
			itasks.Service.INSTANCE = Object.create(itasks.Service);
		}
		return itasks.Service.INSTANCE;
	}
};

//Abstract connection, for now a combination of an eventsource and separate requests to send events
itasks.Connection = {
	eventSource: null,
	taskUrl: '',
	taskInstances: {},

	connect: function() {
		var me = this;
		if(me.eventSource !== null) {
			return;
		}
		me.eventSource = new EventSource(me.taskUrl + '/gui-stream?instances='+Object.keys(me.taskInstances).join(','));
        me.eventSource.onerror = me.onError_.bind(me);
        me.eventSource.addEventListener('reset', me.onReset_.bind(me), false);
        me.eventSource.addEventListener('message', me.onMessage_.bind(me), false);
	},
	addTaskInstance: function(taskInstance, callback) {
		var me = this;
		me.taskInstances[taskInstance] = callback;

		if(me.eventSource !== null) {
			me.disconnect();
			me.connect();
		}
	},
	removeTaskInstance: function(taskInstance) {
		delete(me.taskInstances[taskInstance]);

		if(me.eventSource !== null) {
			me.disconnect();
			me.connect();
		}
	},
	sendEvent: function(taskInstance, event) {
		var me = this, xhr;

		xhr = new XMLHttpRequest();
		xhr.open('POST', me.taskUrl + '/gui-events', true);
		xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
		xhr.send(me.urlEncode_(event));
	},
	disconnect: function() {
		var me = this;
		if(me.eventSource !== null) {
			me.eventSource.close();
			me.eventSource = null;
		}
	},
	urlEncode_: function(obj) {
    	var parts = [];
		for(var k in obj) {
        	parts.push(k+'='+encodeURIComponent(obj[k]));
		}
    	return parts.join('&');
	},
	onError_: function(e) {
		console.log("ERROR",e);
	},
	onReset_: function(e) {
		console.log("ERROR",e);
	},
	onMessage_: function(e) {
		var me = this,
			msg = JSON.parse(e.data),
			taskInstance = msg.instance,
			change = msg.change;

		if(me.taskInstances[taskInstance]) {
			me.taskInstances[taskInstance](change);
		} 
	}
};

