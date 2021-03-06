itasks.Container = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'container';
	}
};

itasks.Panel = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'panel';
	}

	initDOMEl() {
		var me = this,
			isTab = (me.parentCmp && me.parentCmp.type == 'TabSet');

		//Create header
		if(me.attributes.title && !isTab) {
			me.headerEl = document.createElement('div');
			me.headerEl.classList.add(me.cssPrefix + 'header');
			me.headerEl.innerHTML = '<span>' + me.attributes.title + '</span>';
			me.domEl.appendChild(me.headerEl);
		}

		//Create separate container div
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);

		//Add sizers
		if(me.attributes.resizable) {
			for(var i = 0; i < me.attributes.resizable.length; i++) {
				me.domEl.append(me.createSizer(me.attributes.resizable[i]));
			}
		}	

		//Fullscreenable
		if(me.attributes.fullscreenable){
			me.domEl.style.position = 'relative';
			var fullscreener = document.createElement('a');
			var button = document.createElement('div');
			button.classList.add(me.cssPrefix + "button-icon");
			button.classList.add("icon-fullscreen");
			fullscreener.appendChild(button);
			fullscreener.style.position = 'absolute';
			fullscreener.style.bottom = 0;
			fullscreener.style.right = 0;
			fullscreener.href = '#';

			me.fullscreen = false;
			fullscreener.onclick = function () {
				if(me.fullscreen){
					me.domEl.style.position = 'relative';
					me.domEl.style.top = null;
					me.domEl.style.left = null;
					me.domEl.style.right = null;
					me.domEl.style.bottom = null;
					me.domEl.style.width = null;
					me.domEl.style.height = null;
					fullscreener.style.zIndex = null;
					me.domEl.style.zIndex = null;
					me.fullscreen = false;
				} else {
					me.domEl.style.position = 'absolute';
					me.domEl.style.top = 0;
					me.domEl.style.left = 0;
					me.domEl.style.right = 0;
					me.domEl.style.bottom = 0;
					me.domEl.style.width = '100%';
					me.domEl.style.height = '100%';
					fullscreener.style.zIndex = 999;
					me.domEl.style.zIndex = 998;
					me.fullscreen = true;
				}
			};
			me.domEl.appendChild(fullscreener);
		}
	}
	createSizer(side) {
	
		var me = this,
		    el = document.createElement('div');

		el.classList.add(me.cssPrefix + 'sizer-' + side);
		el.addEventListener('mousedown', function init (e){
			function preventDefault(e) { e.preventDefault(); }
			window.addEventListener('selectstart', preventDefault);

			if(side == 'top' || side == 'bottom') {
				var startPos = e.clientY;
				var startSize = parseInt(window.getComputedStyle(me.domEl).getPropertyValue('height').slice(0,-2));
				me.domEl.style['height'] = startSize + 'px';
			} else {
				var startPos = e.clientX;
				var startSize = parseInt(window.getComputedStyle(me.domEl).getPropertyValue('width').slice(0,-2));
				me.domEl.style['width'] = startSize + 'px';
			}
			//Disable flexing
			me.domEl.style.flex = '0 0 auto';

			var resize = function resize(ev) {
				switch(side) {	
					case 'bottom': me.domEl.style['height'] = (startSize + (ev.clientY - startPos)) + 'px'; break;
					case 'top': me.domEl.style['height'] = (startSize + (startPos - ev.clientY)) + 'px'; break;
					case 'right': me.domEl.style['width'] = (startSize + (ev.clientX - startPos)) + 'px'; break;
					case 'left': me.domEl.style['width'] = (startSize + (startPos - ev.clientX)) + 'px'; break;
				}
				//Trigger resize event on all siblings
				me.parentCmp.children.forEach(function(child) {
					child.onResize();
				});
			};

			window.addEventListener('mousemove', resize, false);
			window.addEventListener('mouseup', function stop(e){
					window.removeEventListener('selectstart', preventDefault);
					window.removeEventListener('mousemove', resize, false);
					window.removeEventListener('mouseup', stop, false);
				}, false);
			}, false);
	
		return el;	
	}
	renderChildren() {
		var me = this;
		me.children.forEach(function(child) {
			if(child.domEl) {
				if(child.domEl.classList.contains("itasks-buttonbar") || child.domEl.classList.contains("itasks-toolbar")) {
					me.domEl.appendChild(child.domEl);
				} else {
					me.containerEl.appendChild(child.domEl);
				}
			}
		});
	}
	addChildToDOM(child, idx) {
		var me = this;
		var is_bar = child.domEl.classList.contains("itasks-buttonbar") || child.domEl.classList.contains("itasks-toolbar") ;
		var container = is_bar ? me.domEl : me.containerEl;

		var insert_before_idx = idx + 1; //The child is already part of the collection, so search from the next element
		var insert_before_element = null

		while(insert_before_element == null && insert_before_idx < me.children.length) {

			var sibling = me.children[insert_before_idx].domEl;
			var sibling_is_bar = sibling.classList.contains("itasks-buttonbar") || sibling.classList.contains("itasks-toolbar") ;
			if ((is_bar && sibling_is_bar) || (!is_bar && !sibling_is_bar)) {
				insert_before_element = sibling;
			} else {
				insert_before_idx++;
			}
		}
		if(insert_before_element == null) {
			container.appendChild(child.domEl);
		} else {
			container.insertBefore(child.domEl, insert_before_element);
		}
	}
	setTitle(title) {
		var me = this;
		if(me.headerEl != null) {
			var titleEl = me.headerEl.getElementsByTagName('span')[0];
			titleEl.textContent = title;
		} else if(me.tabEl != null) {
			var titleEl = me.tabEl.getElementsByTagName('span')[0];
			titleEl.textContent = title;
		}
	}
	onAttributeChange(name,value) {
		if(name == 'title') {
			this.setTitle(value);	
		}	
	}
};
itasks.TabSet = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'tabset';
		this.activeTab = 0;
		this.attributes = Object.assign({
			width: 'flex',
			height: 'flex'
		},this.attributes);
		this.replacing = false;
	}
	initDOMEl() {
        var me = this,
            el = me.domEl;

        me.tabBar = document.createElement('ul');
        me.tabBar.classList.add(me.cssPrefix + 'tabbar');

		//Create tabs for the initial children
		me.children.forEach(function(child,i) {
			me.tabBar.appendChild(me.createTabEl(child, i == me.activeTab));
		});	
		
        me.domEl.appendChild(me.tabBar);

        me.containerEl = document.createElement('div');
        me.containerEl.classList.add(me.cssPrefix + 'tabitems');
        me.domEl.appendChild(me.containerEl);
	}
	createTabEl(cmp, selected) {
		var me = this, tab, label, icon;
		tab = document.createElement('li');
		label = document.createElement('a');
		if (cmp.type == 'Button'){
			label.innerHTML = '<span>'+ (cmp.attributes.text || '-')+'</span>';
			label.href = '#';
			label.addEventListener('click',function(e) {
            	if(cmp.attributes.enabled) {
					cmp.doEditEvent(cmp.attributes.taskId,cmp.attributes.editorId,cmp.attributes.value);
            	}
				e.preventDefault();
			},me);
			if(!cmp.attributes.enabled) {
				tab.classList.add(me.cssPrefix + 'tab-disabled');
			}
			cmp.domEl.style.display = "none";
		} else {
			label.innerHTML = '<span>'+ (cmp.attributes.title || '-')+'</span>';
			label.href = '#';
	
			label.addEventListener('click',function(e) {
				var tabEl = e.target.parentElement.parentElement,	
					tabBar = tabEl.parentElement,
					idx = Array.prototype.indexOf.call(tabBar.children,tabEl);
	
				me.setActiveTab(idx);
				e.preventDefault();
			},me);
		}

		if(cmp.attributes.iconCls) {
			icon = document.createElement('div');
			icon.classList.add(me.cssPrefix + 'tabicon');
			icon.classList.add(cmp.attributes.iconCls);
			label.insertBefore(icon,label.childNodes[0]);
		}
		tab.appendChild(label);
	
		if (cmp.type !== 'Button'){
			if(cmp.attributes.closeTaskId) {
				let closeLink = document.createElement('a');
				closeLink.innerHTML = 'x';
				closeLink.href = '#';
				closeLink.classList.add(me.cssPrefix + 'tabclose');
				closeLink.addEventListener('click',function(e) {
					me.doEditEvent(cmp.attributes.closeTaskId,null,'Close');
					e.preventDefault();
				},me);
	
				tab.appendChild(closeLink);
			}
			if(selected) {
				tab.classList.add(me.cssPrefix + 'selected');
			}
		}

		//Hide the tab for children that are windows
		if(cmp.type === 'Window') {
			tab.style.display = 'none';
		}

		//Add a reference to the tab on the related child element
		cmp.tabEl = tab;

		return tab;
	}
	setActiveTab(idx) {
        var me = this;

		//Deselect previously selected tab
        if(me.children[me.activeTab]) {
            me.children[me.activeTab].domEl.classList.remove(me.cssPrefix + 'selected');
            me.tabBar.children[me.activeTab].classList.remove(me.cssPrefix + 'selected');
			me.children[me.activeTab].onHide();
        }
		
        me.activeTab = idx || 0;

		//Select new tab 
        if(me.children[me.activeTab]) {
            me.children[me.activeTab].domEl.classList.add(me.cssPrefix + 'selected');
            me.tabBar.children[me.activeTab].classList.add(me.cssPrefix + 'selected');
			me.children[me.activeTab].onShow();
        }
	}
	setActiveTabBasedOnOrder() {
		var me = this,
			maxOrder = 0,
			maxIndex = 0;

		me.children.forEach(function(child,n) {
			var order = child.attributes["order"] || 0;
			if(order > maxOrder) {
				maxOrder = order;
				maxIndex = n;
			}
		});
		me.setActiveTab(maxIndex);
	}
	beforeChildInsert(idx,spec) {
		var me = this;

		//Overwrite size to always be flex items
		spec.width = 'flex';
		spec.height = 'flex';
	}
	afterChildInsert(idx) {
		var me = this,
			child = me.children[idx];

		//Add tab style
		child.domEl.classList.add(me.cssPrefix + 'tabitem');
		if(idx == me.activeTab) {
			child.domEl.classList.add(me.cssPrefix + 'selected');
		}

		if(me.initialized && !me.replacing) {
			var tabEl = me.createTabEl(child);
			if(idx >= me.tabBar.children.length) {
				me.tabBar.appendChild(tabEl);
			} else {
				me.tabBar.insertBefore(tabEl,me.tabBar.children[idx]);
			}
			me.setActiveTabBasedOnOrder();
		}
	}
	afterChildRemove(idx) {
		var me = this;

		if(me.initialized && !me.replacing) {

			me.tabBar.removeChild(me.tabBar.children[idx]);

			//If we removed the currently active tab, we need to select another one
			if(idx == me.activeTab) {
				me.setActiveTabBasedOnOrder();
			} else if(idx < me.activeTab) {
				//When a tab before the active tab is removed, it implies that the active index
				//is now one less
				me.activeTab--;
			}
		}
	}
	afterChildChange(idx,change) {
		var me = this;
		if(change["attributes"]) {
			me.setActiveTabBasedOnOrder();
		}
	}
	replaceChild(idx,spec) {
		var me = this;
		if(idx >= 0 && idx < me.children.length) {
			return new Promise(function (resolve) {
				me.replacing = true;
				me.removeChild(idx);
				resolve();		
			}).then(function () {
				return me.insertChild(idx,spec);
			}).then(function () {
				me.replacing = false;
			});
		}
	}
};

itasks.Window = class extends itasks.Panel {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'window';
		this.attributes = Object.assign({
			movable: true,
			resizable: true,
			windowType: 'floating',
			hpos: 'center',
			vpos: 'top'
		},this.attributes);
	}
	initDOMEl() {
        var me = this,left,top;

        switch(me.attributes.windowType) {
            case 'bubble':
                me.domEl.classList.add(me.cssPrefix + 'notification-bubble');
                break;
            default:
                me.domEl.classList.add(me.cssPrefix + 'floating-window');
        }
		//Create header
		if(me.attributes.windowType === 'floating' || me.attributes.title) {
			me.headerEl = document.createElement('div');
			me.headerEl.classList.add(me.cssPrefix + 'header');
			me.headerEl.innerHTML = '<span>' + (me.attributes.title || '') + '</span>';
			me.domEl.appendChild(me.headerEl);

        	if(me.attributes.movable) {
            	me.headerEl.addEventListener('mousedown', me.onStartDrag.bind(me));
	            me.headerEl.style.cursor = 'move';
			}
		}
		if(me.attributes.resizable) {
			me.domEl.style.resize = 'both';
			me.domEl.style.overflow = 'scroll';
		}
		//Create separate container div
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);

		//Intially position the window offscreen, it will be repositioned once its dimensions are known
		me.domEl.style.top = '-10000px';
		me.domEl.style.left = '-10000px';
	}
	initSize() {
	}
	onShow() {
		//Position the window when it is is first shown
		var me = this, top, left;

		if(!me.positioned) {	
			switch(me.attributes.vpos) {
				case 'top': top = me.attributes.marginTop; break;
				case 'middle': top = (document.body.offsetHeight / 2) - (me.domEl.offsetHeight / 2); break;
				case 'bottom': top = document.body.offsetHeight - me.domEl.offsetHeight - me.attributes.marginBottom; break;
			}
			switch(me.attributes.hpos) {
				case 'left': left = me.attributes.marginLeft; break;
				case 'center': left = (document.body.offsetWidth / 2) - (me.domEl.offsetWidth / 2); break;
				case 'right': left = document.body.offsetWidth - me.domEl.offsetWidth - me.attributes.marginRight; break;
			}

			me.domEl.style.top = top + 'px';
			me.domEl.style.left = left + 'px';
			me.positioned = true;
		}
	}
	onStartDrag(e) {
        var me = this;
        e.preventDefault();
        me.lastX = e.clientX;
        me.lastY = e.clientY;
        me.onDragging_ = me.onDragging.bind(me);
        me.onStopDrag_ = me.onStopDrag.bind(me);
        window.addEventListener('mousemove', me.onDragging_);
        window.addEventListener('mouseup', me.onStopDrag_);
	}
	onDragging(e) {
        var me = this,
            newX = e.clientX,
            newY = e.clientY,
            diffY = newY - me.lastY,
            diffX = newX - me.lastX,
            left, top;

        left = parseInt(document.defaultView.getComputedStyle(me.domEl,'').getPropertyValue('left'),10);
        top = parseInt(document.defaultView.getComputedStyle(me.domEl,'').getPropertyValue('top'),10);
        me.domEl.style.left = ((left < 0) ? 0 : (left + diffX)) + 'px';
        me.domEl.style.top = ((top < 0) ? 0 : (top + diffY)) + 'px';

        me.lastX = newX;
        me.lastY = newY;
	}
	onStopDrag(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
};

itasks.ToolBar = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'toolbar';
	}
};

itasks.ButtonBar = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'buttonbar';
	}
};
itasks.List = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'list';
	}
};
itasks.ListItem = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'listitem';
	}
};
itasks.Debug = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'debug';
	}
};

itasks.Menu = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'menu';
	}
	initDOMEl() {
		var me = this;

		if (!(me.parentCmp.type != 'Menu' && me.children.length == 1)){
			me.labelEl = document.createElement('div');
			me.labelEl.classList.add(me.cssPrefix + 'menu-label');
			me.innerLabelEl = document.createElement('span');
			me.innerLabelEl.innerHTML = me.attributes.text;

			if(me.attributes.iconCls) {
				me.icon = document.createElement('div');
				me.icon.classList.add(me.cssPrefix + 'button-icon');
				me.icon.classList.add(me.attributes.iconCls);
				me.labelEl.appendChild(me.icon);
			}

			me.labelEl.appendChild(me.innerLabelEl);

			me.domEl.appendChild(me.labelEl);

			me.containerEl = document.createElement('div');
			me.containerEl.classList.add(me.cssPrefix + 'menu-content');
			me.domEl.appendChild(me.containerEl);
		}
	}
};

itasks.MenuSep = class extends itasks.Component {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'menu-sep';
	}
};
