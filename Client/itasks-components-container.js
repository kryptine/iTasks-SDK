itasks.Container = {
	cssCls: 'container',
	initDOMEl: function() {
		if(this.baseCls) {
			this.domEl.classList.add(this.baseCls);
		}
	}
};
itasks.Panel = {
	cssCls: 'panel',
	initDOMEl: function() {
		var me = this;
		//Create header
		if(me.title) {
			me.headerEl = document.createElement('div');
			me.headerEl.classList.add(me.cssPrefix + 'header');
			me.headerEl.innerHTML = '<span>' + me.title + '</span>';
			me.domEl.appendChild(me.headerEl);
		}
		//Create separate container div
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);

		if(me.frame) {
			me.domEl.classList.add(me.cssPrefix + 'framed');
		}
		if(me.baseCls) {
			me.domEl.classList.add(me.baseCls);
		}
	}
};
itasks.TabSet = {
	cssCls: 'tabset',
    activeTab: 0,
	height: 'flex',
	width: 'flex',

	initComponent: function() {
		var me = this;
		me.children.forEach(function(child,i) {
			child.selected = (i == me.activeTab);
		});
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.tabBar = document.createElement('ul');
        me.tabBar.classList.add(me.cssPrefix + 'tabbar');

		//Create tabs for the initial children
		me.children.forEach(function(child,i) {
			me.tabBar.appendChild(me.createTabEl(child));
		});	
		
        me.domEl.appendChild(me.tabBar);

        me.containerEl = document.createElement('div');
        me.containerEl.classList.add(me.cssPrefix + 'tabitems');
        me.domEl.appendChild(me.containerEl);
    },
	createTabEl: function (cmp) {
		var me = this, tab, label, icon;

		tab = document.createElement('li');
        label = document.createElement('a');
		label.innerHTML = '<span>'+ (cmp.title || '-')+'</span>';
		label.href = '#';

		label.addEventListener('click',function(e) {
			var tabEl = e.target.parentElement.parentElement,	
				tabBar = tabEl.parentElement,
				idx = Array.prototype.indexOf.call(tabBar.children,tabEl);

			me.setActiveTab(idx);
            e.preventDefault();
		},me);

        if(cmp.iconCls) {
            icon = document.createElement('div');
            icon.classList.add(me.cssPrefix + 'tabicon');
            icon.classList.add(cmp.iconCls);
            label.insertBefore(icon,label.childNodes[0]);
        }
        tab.appendChild(label);

        if(cmp.closeTaskId) {
            closeLink = document.createElement('a');
            closeLink.innerHTML = 'x';
            closeLink.href = '#';
            closeLink.classList.add(me.cssPrefix + 'tabclose');
            closeLink.addEventListener('click',function(e) {
                me.doActionEvent(cmp.closeTaskId,'Close');
                e.preventDefault();
            },me);

            tab.appendChild(closeLink);
        }
        if(cmp.selected) {
            tab.classList.add(me.cssPrefix + 'selected');
        }
		return tab;
	},
	setActiveTab: function(idx) {
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
	},
	beforeChildInsert: function(idx,spec) {
		var me = this;

		//Overwrite size to always be flex items
		spec.width = 'flex';
		spec.height = 'flex';
	},
	afterChildInsert: function(idx) {
		var me = this,
			child = me.children[idx];

		//Add tab style
		child.domEl.classList.add(me.cssPrefix + 'tabitem');
		if(child.selected) {
			child.domEl.classList.add(me.cssPrefix + 'selected');
		}

		if(me.initialized) {
			var tabEl = me.createTabEl(child);
			if(idx >= me.tabBar.children.length) {
				me.tabBar.appendChild(tabEl);
			} else {
				me.tabBar.insertBefore(tabEl,me.tabBar.children[idx]);
			}

			if(me.children.length == 1) { //Automatically select the first tab
				me.setActiveTab(idx);
			}

		}
	},
	beforeChildRemove: function(idx) {
		var me = this;
		if(me.initialized) {
			if((idx == me.activeTab) && (me.children.length > 1)) { //Unless we remove the last tab, select another tab
				me.setActiveTab( (idx == 0) ? 1 : (idx - 1));
			}
			me.tabBar.removeChild(me.tabBar.children[idx]);
		}
	}
}

itasks.Window = {
    margins: '10 10 10 10',
	movable: false,
	modal: false,
	windowType: 'floating',

    initDOMEl: function() {
        var me = this;
        switch(me.windowType) {
            case 'modal':
                me.modal = true;
                me.domEl.classList.add(me.cssPrefix + 'modal-window');
                break;
            case 'bubble':
                me.domEl.classList.add(me.cssPrefix + 'notification-bubble');
                break;
            default:
                me.movable = true;
                me.domEl.classList.add(me.cssPrefix + 'floating-window');
        }
/*
        me.setTitle(me.definition.title);
        me.setCloseTaskId(me.definition.closeTaskId);
*/
        if(me.movable && me.headerEl) {
            me.headerEl.addEventListener('mousedown', me.onStartDrag.bind(me));
            me.headerEl.style.cursor = 'move';
        }
    },
    initSize: function() {
/*
        var me = this,
            el = me.panelEl,
            width = me.definition.itwcWidth || me.defaultWidth,
            height = me.definition.itwcHeight || me.defaultHeight;

        if(width != 'flex' && width != 'wrap') {
            el.style.width = width + 'px';
            el.style.minWidth = width + 'px';
        }
        if(height != 'flex' && height != 'wrap') {
            el.style.height = height + 'px';
            el.style.minHeight = height + 'px';
        }
        if(me.modal) {
            me.domEl.style.width = window.innerWidth + 'px';
            me.domEl.style.height = window.innerHeight + 'px';
        }
*/
    },
    onStartDrag: function(e) {
        var me = this;

        e.preventDefault();
        me.lastX = e.clientX;
        me.lastY = e.clientY;
        me.onDragging_ = me.onDragging.bind(me);
        me.onStopDrag_ = me.onStopDrag.bind(me);
        window.addEventListener('mousemove', me.onDragging_);
        window.addEventListener('mouseup', me.onStopDrag_);
    },
    onDragging: function(e) {
        var me = this,
            newX = e.clientX,
            newY = e.clientY,
            diffY = newY - me.lastY,
            diffX = newX - me.lastX,
            left, top;

        left = parseInt(document.defaultView.getComputedStyle(me.panelEl,'').getPropertyValue('left'),10);
        top = parseInt(document.defaultView.getComputedStyle(me.panelEl,'').getPropertyValue('top'),10);
        me.domEl.style.left = ((left < 0) ? 0 : (left + diffX)) + 'px';
        me.domEl.style.top = ((top < 0) ? 0 : (top + diffY)) + 'px';

        me.lastX = newX;
        me.lastY = newY;
    },
    onStopDrag: function(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
};
itasks.ToolBar  = {
	cssCls: 'toolbar',
	height: 'wrap',
	width: 'flex',
	direction: 'horizontal',
	halign: 'left',
	padding: '2 2 2 2'
};

itasks.ButtonBar  = {
	cssCls: 'buttonbar',
	height: 'wrap',
	width: 'flex',
	direction: 'horizontal',
	halign: 'right',
	padding: '2 2 2 0'
};
itasks.Debug = {
	cssCls: 'debug'
};
