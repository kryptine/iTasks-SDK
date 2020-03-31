//Global itasks namespace
itasks = {};

const ABC_loading_promise=ABCInterpreter.instantiate({
	bytecode_path: '/js/app.pbc',

	heap_size: 8<<20,
	stack_size: 512<<10,

	encoding: 'utf-8',

	with_js_ffi: true,
}).then(instance => { itasks.ABC=instance; });

//Auxiliary definitions for sending Maybe values to server
const Nothing = ["Nothing"];
function Just(x) { return["Just", x]; }

//Global lookup table of itask components indexed by their dom element.
//This makes it possible to find the managing itask component object for arbitrary dom elements.
//Because it is a WeakMap, we can register components without having to unregister them.
itasks.components = new WeakMap();

//Core behavior
itasks.Component = class {
	constructor(spec, parentCmp = null) {
		this.domTag = 'div';
		this.domEl = null;

		this.container = true;
		this.containerEl = null;

		this.cssPrefix = 'itasks-';
		this.cssCls = 'component';

		this.parentCmp = parentCmp;
		this.attributes = {};
		this.children = [];

		this.initialized = false;

		this.shared_clean_values = null;

		//Initialize object attributes from spec
		Object.assign(this, spec);
	}
	init() {
		var me = this;
		me.lastFire = 0;
		return me.world=Promise.resolve()
			.then(me.initUI.bind(me))
			.then(me.initComponent.bind(me))
			.then(me.initChildren.bind(me))
			.then(me.renderComponent.bind(me))
			.then(me.registerComponent.bind(me))
			.then(function(){ me.initialized=true; });
	}
	initUI() {
		var me=this;
		if (me.attributes.initUI!=null && me.attributes.initUI!='') {
			return ABC_loading_promise.then(function(){
				var initUI=itasks.ABC.deserialize(atob(me.attributes.initUI));
				var ref=itasks.ABC.share_clean_value(initUI,me);
				itasks.ABC.interpret(new SharedCleanValue(ref), [me, itasks.ABC.initialized ? 0 : 1]);
				itasks.ABC.clear_shared_clean_value(ref);
			});
		}
	}
	initComponent() { //Abstract method: every component implements this differently
	}
	initChildren() {
		var me = this;
		return me.children.reduce((promise,spec,i) => promise.then(function(){
			me.beforeChildInsert(i,spec);
			me.children[i] = me.createChild(spec);
			return me.children[i].init().then(function(){
				me.afterChildInsert(i,me.children[i]);
			});
		}), Promise.resolve());
	}
	renderComponent() {
		var me = this;
		if(me.domEl === null) { //Create a new dom element
			me.domEl = document.createElement(me.domTag);
		} else { //Clear an existing element
			me.domEl.innerHTML = '';
		}
		//Initially make the outer dom element also the container element that holds the child components
		me.containerEl = me.domEl;
			
		//Style the dom element
		me.domEl.classList.add(me.cssPrefix + me.cssCls);
		if(me.attributes['style']) {
			me.domEl.style = me.attributes['style'];
		}
		if(me.attributes['class']) {
			if(Array.isArray(me.attributes['class'])) {
				var len = me.attributes['class'].length;
				for(var i = 0; i < len; i++) {
					me.domEl.classList.add(me.attributes['class'][i]);
				}
			} else {
				me.domEl.classList.add(me.attributes['class']);
			}
		}
		//Custom initialization after the dom element has been rendered
		me.initDOMEl();
		//Size the element if explicit sizes are given
		me.initDOMElSize();
		//Render the child elements
		me.renderChildren();
	}
	renderChildren() {
		var me = this;
		me.children.forEach(function(child) {
			if(child.domEl) {
				me.containerEl.appendChild(child.domEl);
			}
		});
	}
	registerComponent() {
		if(this.domEl !== null) {
			itasks.components.set(this.domEl,this);
		}
	}
	initDOMEl() {
	}
	initDOMElSize() {
		var me = this,
			el = me.domEl,
			width = me.attributes.width,
			height = me.attributes.height;

		//Set width & height using attributes
		if('width' in me.attributes) {
			if(width === 'flex') {
				el.classList.add(me.cssPrefix + 'flex-width');
			} else if (width === 'wrap') {
				el.classList.add(me.cssPrefix + 'wrap-width');
			} else {
				el.classList.add(me.cssPrefix + 'exact-width');
				el.style.width = width + 'px';
			}
		}
		if('height' in me.attributes) {
			if(height === 'flex') {
				el.classList.add(me.cssPrefix + 'flex-height');
			} else if (height === 'wrap') {
				el.classList.add(me.cssPrefix + 'wrap-height');
			} else {
				el.classList.add(me.cssPrefix + 'exact-height');
				el.style.height = height + 'px';
			}
		}
	}
	doEditEvent(taskId, editorId, value) {
		var me = this;
		if(me.parentCmp) {
			//Timeout is set, check if we can fire
			var now = (new Date()).getTime();
			//We are within timeout
			if (me.attributes.eventTimeout && now - me.lastFire < me.attributes.eventTimeout){
				window.clearTimeout(me.queuedFire);
				me.queuedFire = window.setTimeout(function (){
					me.doEditEvent(taskId, editorId, value);
				}, me.attributes.eventTimeout - (now - me.lastFire));
			} else {
				me.lastFire = now;
				me.parentCmp.doEditEvent(taskId, editorId, value);
			}
		}
	}
	findChild(obj) {
		var me = this, num = me.children.length, i;

		for( i = 0; i < num; i++) {
			if(me.children[i] === obj) {
				return i;
			}
		}
	}
	createChild(spec) {
		return new itasks[spec.type || 'Component'](spec,this);
	}
	insertChild(idx = 0, spec = {}) {
		var me = this,
			child = null,
			isLast = (idx == me.children.length);
		
		me.beforeChildInsert(idx,spec);

		//Create the child object
		child = me.initialized ? me.createChild(spec) : spec ;

		//Add the child to the collection of children
		me.children.splice(idx,0,child);

		var finish_up=function(){
			me.afterChildInsert(idx,child);
			if (child.onResize)
				child.onResize();
		};

		if(me.initialized) {
			//Initialize, if we are already initialized
			return child.init().then(function(){
				//Add the child to the dom
				if(child.domEl) {
					me.addChildToDOM(child,idx);
					child.onShow();
				}
				finish_up();
			});
		} else {
			finish_up();
		}
	}
	//Separate method for inserting the child in the DOM.
	//This enables subclasses to put some nodes in a different place than in `containerEl`
	addChildToDOM(child, idx) {
		var me = this,
			isLast = (idx == me.children.length);
		if(isLast) {
			me.containerEl.appendChild(child.domEl);
		} else {
			me.containerEl.insertBefore(child.domEl,me.containerEl.childNodes[idx]);
		}
	}
	beforeChildInsert(idx,spec) {
	}
	afterChildInsert(idx,child) {
	}
	removeChild(idx = 0) {
		var me = this, child = me.children[idx];

		child._beforeRemove();
		me.beforeChildRemove(idx,child);

		if(me.initialized && child.domEl) {
			child.domEl.parentNode.removeChild(child.domEl);
		}
		me.children.splice(idx,1);	
		me.afterChildRemove(idx);
	}
	replaceChild(idx,spec) {
		var me = this;
		if(idx >= 0 && idx < me.children.length) {
			me.removeChild(idx);
			return me.insertChild(idx,spec);
		}
	}
	moveChild(sidx,didx) {
		var me = this, child;

		if(me.initialized && me.children[sidx].domEl) {
			if(didx == (me.containerEl.children.length - 1)) {
				me.containerEl.appendChild(me.containerEl.children[sidx]);
			} else {
				me.containerEl.insertBefore(me.containerEl.children[sidx],me.containerEl.children[(didx > sidx) ? (didx + 1) : didx]);
			}
		}
		child = me.children.splice(sidx,1)[0]; //Remove followed by insert...
		me.children.splice(didx, 0, child);
	}
	beforeChildRemove(idx,child) {
	}
	afterChildRemove(idx) {
	}
	/* beforeRemove can be overwritten to add a handler for 'destroy' events.
	 * _beforeRemove is internal and should not be overwritten.
	 */
	beforeRemove() {
	}
	_beforeRemove() {
		this.beforeRemove();

		if (this.shared_clean_values!=null) {
			// garbage collect any remaining values shared with wasm
			this.shared_clean_values.forEach(ref => itasks.ABC.clear_shared_clean_value(ref,false));
			this.shared_clean_values.clear();
		}

		this.children.forEach(child => child._beforeRemove());
	}
	setAttribute(name,value) {
		var me = this;
	
		me.attributes[name] = value;
		me._onAttributeChange(name,value);
	}
	_onAttributeChange(name,value) {
		var me = this;
		if(name == 'class') {
			me.domEl.className = me.cssPrefix + me.cssCls;
			if(Array.isArray(value)) {
				value.forEach(function(cls) {
					me.domEl.classList.add(cls);
				});
			} else {
				me.domEl.classList.add(value);
			}
		} else {
			me.onAttributeChange(name,value);
		}
	}
	onAttributeChange(name,value) {
	}
	onUIChange(change) {
		var me = this;
		me.world=me.world.then (function(){
			if(change) {
				switch(change.type) {
					case 'replace':
						return me.onReplaceUI(change.definition);
					case 'change':
						return me.onChangeUI(change.attributes,change.children);
				}
			}
		});
	}
	onReplaceUI(spec) {
		var me = this;
		if(me.parentCmp) {
			var idx = me.parentCmp.findChild(me);
			return me.parentCmp.replaceChild(idx,spec);
		}
	}
	onChangeUI(attributeChanges,childChanges) {
		var me = this;

		//Handle attribute changes
		if(attributeChanges instanceof Array) {
			attributeChanges.forEach(function(change) {
				me.setAttribute(change.name,change.value);
			});
		}
		//Handle child changes
		if (childChanges instanceof Array) {
			childChanges.reduce((promise,change) => promise.then(function(){
				var idx = change[0];
				switch(change[1]) {
					case 'change':
						if(idx >= 0 && idx < me.children.length) {
							me.children[idx].onUIChange(change[2]);
							me.world = me.world.then(function () {
								me.afterChildChange(idx,change[2]);
							});
							return;
						} else {
							console.log("UNKNOWN CHILD",idx,me.children.length,change);
						}
						break;
					case 'insert':
						return me.insertChild(idx,change[2]);
					case 'remove':
						me.removeChild(idx);
						break;
					case 'move':
						me.moveChild(idx,change[2]);
						break;
				}
			}), Promise.resolve());
		}
	}
	afterChildChange(idx,change) {
	}
	onShow() {
		this.children.forEach(function(child) { if(child.onShow) {child.onShow();}});
	}
	onHide() {
		this.children.forEach(function(child) { if(child.onHide) {child.onHide();}});
	}
	onResize() {
		this.children.forEach(function(child) { if(child.onResize) {child.onResize();}});
	}
	onHtmlEvent(msg) {
	}
	getViewport() {
		var me = this, vp = me.parentCmp;
		while(vp) {
			if(vp.cssCls == 'viewport') { //Bit of a hack...
				return vp;
			}
			vp = vp.parentCmp;
		}
		return null;
	}
};

itasks.Loader = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'loader';
	}

	initDOMEl() {
		var me = this,
			l = document.createElement('div');
			l.classList.add(me.cssPrefix + 'loader-spinner');
		me.domEl.appendChild(l);
		//Temporary
		me.domEl.classList.add(me.cssPrefix + 'flex-width');
		me.domEl.classList.add(me.cssPrefix + 'flex-height');
		if(me.attributes.taskId){
			me.doEditEvent(me.attributes.taskId, me.attributes.editorId, Just (true));
		}
	}
};

itasks.ExceptionView = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'exception';
		this.container = false;
	}

	initDOMEl() {
		//Temporary
		this.domEl.classList.add(this.cssPrefix + 'flex-width');
		this.domEl.classList.add(this.cssPrefix + 'flex-height');
		this.domEl.innerHTML = '<h1>Exception</h1><span>' + (this.attributes.value || '') + '</span>';
	}
};

itasks.Viewport = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'viewport';
		this.syncTitle = spec.syncTitle || false;
	}
	initComponent() {
		var me = this;

		//Create a temporary root element
		me.insertChild(0,{type:'Loader'});

		me.changeListeners = [];
		me.parentViewport = me.getViewport();

		if(me.parentViewport == null) {
			window.addEventListener('resize',me.onResize.bind(me));
		}
		//Connect the viewport to a task url
		me.connect();
	}
	connect() {
		//Get a connection
		this.taskUrl = this.determineTaskEndpoint();
		this.connection = itasks.ConnectionPool.getConnection(this.taskUrl);

		const startCallback = this.onSessionStart.bind(this);
		const uiChangeCallback = this.onTaskUIChange.bind(this);
		const removedCallback = this.onTaskRemoved.bind(this);
		const exceptionCallback = this.onException.bind(this);

		if('instanceNo' in this.attributes && 'instanceKey' in this.attributes) {
			//Connect to an existing task instance
			this.connection.attachTaskInstance(this.attributes['instanceNo'],this.attributes['instanceKey'],
				uiChangeCallback,removedCallback,exceptionCallback);
		} else {
			//Create a new session
			this.connection.newSession(startCallback,uiChangeCallback,removedCallback,exceptionCallback);
		}
	}
	determineTaskEndpoint() {
		var me = this,
			https = location.protocol.startsWith('https');

		if(me.taskUrl) { //Something was configured explicitly
			if(me.taskUrl.startsWith('ws://') || me.taskUrl.startsWith('wss://')) {
				return me.taskUrl;
			} else {
				
				return (https ? 'wss://' : 'ws://') + location.host + me.taskUrl + (me.taskUrl.endsWith('/') ? '' : '/') + 'gui-wsock';
			}
		} 
		if(me.parentViewport === null) {
			//If there is no parent, use the default url
			return (https ? 'wss://' : 'ws://') + location.host + location.pathname + (location.pathname.endsWith('/') ? '' : '/') + 'gui-wsock';
		} else {
			return me.parentViewport.determineTaskEndpoint();
		}
	}
	doEditEvent(taskId, editorId, value) {
		var me = this, taskNo = taskId.split("-")[1];
		if(editorId) {
			me.connection.sendEditEvent(me.attributes.instanceNo, taskNo, editorId, value);
		} else {
			me.connection.sendActionEvent(me.attributes.instanceNo, taskNo, value);
		}
	}
	onSessionStart(instanceNo,instanceKey) {
		this.attributes['instanceNo'] = instanceNo;
		this.attributes['instanceKey'] = instanceKey;
	}
	onTaskUIChange(change) {
		var me = this;

		me.children[0].onUIChange(change);
		//Sync title of the top level element
		if(me.syncTitle) {
			if(change.type == 'replace' && 'attributes' in change.definition && 'title' in change.definition.attributes) {
				document.title = change.definition.attributes.title;
			}
			if(change.type == 'change' && change.attributes instanceof Array) {
				change.attributes.forEach(function(change) {
					if(change.name == 'title') {
						document.title = change.value;
					}
				});
			}
		}
		//Trigger changelisteners
		me.changeListeners.forEach(function(cl) {
			cl.onViewportChange(change);
		});
	}
	onTaskRemoved(instanceNo) {
		//Remove all children
		for(var i = this.children.length - 1; i >= 0; i--) {
			this.removeChild(i);
		}
		//Make sure we don't try to restore the current task instance
		delete this.attributes['instanceNo'];
		delete this.attributes['instanceKey'];

		//Create a temporary loader element
		this.insertChild(0,{type:'Loader'});

		//Reconnect
		this.connect();
	}
	onException(exception) {
		var me = this;
		//Remove all children and show the exception message
		for(var i = me.children.length - 1; i >= 0; i--) {
			me.removeChild(i);
		}
		me.insertChild(0,{type:'ExceptionView', attributes: { value : exception}});
	}

	addChangeListener(cmp) {
		var me = this;
		me.changeListeners.push(cmp);
	}
	removeChangeListener(cmp) {
		var me = this;
		me.changeListeners = me.changeListeners.filter(function(el) {
			return el != cmp;
		});
	}
	beforeRemove() {
		var me = this, instanceNo = me.attributes['instanceNo'];

		if(instanceNo) {
			me.connection.detachTaskInstance(instanceNo);
		}
	}
	_beforeRemove() {
		this.beforeRemove();
	}
};

//Data components are elements in the tree that don't render themselves, but make it possible to
//use the generic incremental change mechanism to update parts of a Component
//This can be used for example to incrementally update the list of options in a dropdown component
itasks.Data = class {
	constructor(spec) {
		Object.assign(this, spec);
	}
	init() {
		return Promise.resolve();
	}
	beforeRemove() {
	}
	_beforeRemove() {
	}
};

//Convenience function for concisely creating viewports
itasks.viewport = function(spec,domEl) {
	var vp = new itasks.Viewport(spec);
	vp.domEl = domEl;
	vp.init();
};

itasks.Connection = class {
	constructor(endpointUrl) {
		this.endpointUrl = endpointUrl;
		this.wsock = null;
		this.pingTimer = null;
		this.pingInterval = 10000; //10 seconds

		this.newSessionCallbacks = {};
		this.taskInstanceCallbacks = {};

		this.reqId = 0;
		this.deferred = [];
	}
	connect() {
		var me = this;

		me.wsock = new WebSocket(me.endpointUrl);
		me.wsock.onopen = function() {
			//First send deferred messages
			me.deferred.forEach(function(msg) { me.wsock.send(msg);});
			//Start ping timer
			me.pingTimer = window.setInterval(me._ping.bind(me), me.pingInterval);
		};	
		me.wsock.onmessage = me.onMessage_.bind(me);
		me.wsock.onerror = me.onError_.bind(me);
		me.wsock.onclose = me.onClose_.bind(me);
	}
	_ping() {
		var me = this, reqId = me.reqId++;
		if(me._isConnected()) {
			me.wsock.send(JSON.stringify([parseInt(reqId),'ping',{}]));
		}
	}
	_send(msg) {
		var me = this;	
		if(me._isConnected()) {
			me.wsock.send(JSON.stringify(msg));
		} else {
			me.deferred.push(JSON.stringify(msg));
		}
	}
	_isConnected() {
		return (this.wsock !== null && this.wsock.readyState == 1)
	}
	newSession(onStart, onUIChange, onRemoved, onException) {
		var me = this, reqId = me.reqId++;
			
		me.newSessionCallbacks[reqId] = 
			{ 'onStart' : onStart
			, 'onUIChange' : onUIChange
			, 'onRemoved' : onRemoved
			, 'onException' : onException
			};

		me._send([parseInt(reqId),'new',{}]);
	}
	attachTaskInstance(instanceNo, instanceKey, onUIChange, onRemoved, onException) {
		var me = this, reqId = me.reqId++;

		me.taskInstanceCallbacks[instanceNo] =
			{ 'onUIChange' : onUIChange
			, 'onRemoved' : onRemoved
			, 'onException' : onException
			};

		me._send([parseInt(reqId),'attach',{'instanceNo':parseInt(instanceNo),'instanceKey': instanceKey}]);
	}
	detachTaskInstance(instanceNo) {
		var me = this, reqId = me.reqId++;

		delete(me.taskInstanceCallbacks[instanceNo]);
		me._send([parseInt(reqId),'detach',{'instanceNo':parseInt(instanceNo)}]);
	}
	sendEditEvent(instanceNo, taskNo, edit, value) {
		var me = this, reqId = me.reqId++;
		me._send([parseInt(reqId),'ui-event',{'instanceNo':parseInt(instanceNo),'taskNo':parseInt(taskNo),'edit':edit, 'value': value}]);
	}
	sendActionEvent(instanceNo, taskNo, action) {
		var me = this, reqId = me.reqId++;
		me._send([parseInt(reqId),'ui-event',{'instanceNo':parseInt(instanceNo),'taskNo':parseInt(taskNo),'action': action}]);
	}
	disconnect() {
		var me = this;
		if(me.pingTimer !== null) {
			window.clearInterval(me.pingTimer);
			me.pingTimer = null;
		}
		if(me.wsock !== null) {
			me.wsock.close();
			me.wsock = null;
		}
	}
	onError_(e) {
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException(e);});
	}
	onReset_(e) {
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException(e);});
	}
	onClose_(e) {
		//If there are still attached task instances, we consider it an exeption for those viewports
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException("The connection to the server closed unexpectedly");});
	}
	onMessage_(e) {
		var me = this,
			msg = JSON.parse(e.data);

		//Check the message is a triplet
		if(msg instanceof Array && msg.length == 3) {
			var reqId = msg[0], reqType = msg[1], reqArgs = msg[2];

			switch(reqType) {
				case 'new':
					if(reqId in me.newSessionCallbacks) {
						var callbacks = me.newSessionCallbacks[reqId];

						callbacks.onStart(reqArgs.instanceNo, reqArgs.instanceKey);
						me.attachTaskInstance(reqArgs.instanceNo, reqArgs.instanceKey, callbacks.onUIChange, callbacks.onRemoved, callbacks.onException);

						delete me.newSessionCallbacks[reqId];
					}
					break;
				case 'removed':
					if ('instanceNo' in reqArgs && reqArgs.instanceNo in me.taskInstanceCallbacks) {
						me.taskInstanceCallbacks[reqArgs.instanceNo].onRemoved(reqArgs.instanceNo);
						delete me.taskInstanceCallbacks[reqArgs.instanceNo];
					}
					break;
				case 'revoked':
					if ('instanceNo' in reqArgs && reqArgs.instanceNo in me.taskInstanceCallbacks) {
						me.taskInstanceCallbacks[reqArgs.instanceNo].onRemoved(reqArgs.instanceNo);
						delete me.taskInstanceCallbacks[reqArgs.instanceNo];
					}
					break;
				case 'ui-change':
					if('instanceNo' in reqArgs && 'change' in reqArgs && reqArgs.instanceNo in me.taskInstanceCallbacks) {
						me.taskInstanceCallbacks[reqArgs.instanceNo].onUIChange(reqArgs.change);
					} 
					break;
				case 'set-cookie':
					if('name' in reqArgs && 'value' in reqArgs && 'max-age' in reqArgs) {
						document.cookie = reqArgs['name'] + '=' + reqArgs['value'] + (reqArgs['max-age'] === null ? '' : ';max-age='+reqArgs['max-age']);
					}
					break;
				case 'exception':
					if('instanceNo' in reqArgs && 'description' in reqArgs) {
						//The exception targeted at one task instance, notify its viewport
						if (reqArgs.instanceNo in me.taskInstanceCallbacks) {
							me.taskInstanceCallbacks[reqArgs.instanceNo].onException(reqArgs.description);
							delete me.taskInstanceCallbacks[req.instanceNo];
						} 
					} else {
						//The exception is not specific for one task instance, so notify all viewports
						for(let instanceNo in me.taskInstanceCallbacks) {
							me.taskInstanceCallbacks[instanceNo].onException(reqArgs.description);
							delete me.taskInstanceCallbacks[instanceNo];
						}
					}
					break;
			}
		}
	}
};

itasks.ConnectionPool = {
	connections: {}, //Multiple connections, indexed by endpointUrl

	getConnection: function (endpointUrl) {
		var me = this, connection;

		//Reuse an existing connection
		if(me.connections[endpointUrl]) {
			return me.connections[endpointUrl];
		} else {
			//Create the connection
			connection = new itasks.Connection(endpointUrl);
			connection.connect();
			me.connections[endpointUrl] = connection;
			return connection;
		}
	}
};

//Global functions that you can use to trigger edit events from pieces of html code displayed by components
itasks.htmlEvent = function(event, msg) {
	var domEl = event.target;
	var component = null;

	event.preventDefault();

	while(domEl !== null && (component = itasks.components.get(domEl)) == null) {
		domEl = domEl.parentElement;
	}
	if(component !== null) {
		component.onHtmlEvent(msg);
	}
}
