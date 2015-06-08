//#### iTasks Web Client ####//
//This javascript program defines the web-based run-time environment of iTasks programs
itwc = itwc || {};
itwc.global = {};

//#### UTILITY FUNCTIONS ####//
itwc.util = {};
itwc.util.urlEncode = function (obj) {
    var parts = [];
    for(k in obj) {
        parts.push(k+'='+encodeURIComponent(obj[k]));
    }
    return parts.join('&');
};
//Define a new prototype object by extending the prototype of an existing one
itwc.extend = function(inheritFrom,definition) {
    var c = function() {};
    c.prototype = new inheritFrom();
    for(k in definition) {
        c.prototype[k] = definition[k];
    }
    return c;
}
//#### GENERIC UI COMPONENT BASE DEFINITIONS ####//
itwc.Component = function() {};
itwc.Component.prototype = {
    isContainer: false,

    defaultWidth: 'flex',
    defaultHeight: 'wrap',
    defaultMargins: [0,0,0,00],

    init: function(definition, parentCmp) {
        var me = this;

        me.domEl = null;
        me.targetEl = null;
        me.parentCmp = parentCmp || null;
        me.items = [];
        me.processDefinition(definition);
        me.hotkeyListener = null;
    },
    processDefinition: function(definition) {
        var me = this;
        //Initialize component properties
        //from the definition
        me.definition = definition || {};
        me.type = (definition && definition.xtype) || 'itwc_component';

        //Parse margins
        if(definition.margins) {
            me.margins = definition.margins.split(' ').map(function(m) { return (m | 0) });
        } else {
            me.margins = me.defaultMargins;
        }
    },
    render: function(itemIdx, isLast) {
        var me = this;
        me.domId = 'itwc-' + itwc.DOMID++;

        //Create and initialize dom element
        me.domEl = document.createElement(me.domTag);
        me.targetEl = me.domEl;

        me.initDOMEl(itemIdx);
        me.initSize(itemIdx, isLast);
        me.domEl.id = me.domId;
    },
    //Configurable properties for initializing the DOM element
    domTag: 'div',
    initDOMEl: function(itemIdx) {
    },
    initSize: function(itemIdx,isLast) {
        var me = this,
            el = me.domEl,
            width = me.definition.itwcWidth || me.defaultWidth,
            height = me.definition.itwcHeight || me.defaultHeight,
            direction = me.parentCmp.definition.direction || me.parentCmp.defaultDirection;

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
                el.style.flexShrink = 0;
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
                el.style.flexShrink = 0;
            }
        } else {
            el.style.height = height + 'px';
            el.style.minHeight = height + 'px';
        }
        //Set margins
        me.initMargins(itemIdx,isLast);

        //Set padding
        if(me.definition.padding) {
            el.style.padding = me.definition.padding.split(' ').map(function(s) { return s + 'px'}).join(' ');
        }
        //If a container, set alignment
        if(me.isContainer) {
            me.initItemLayout();
        }
    },
    initMargins: function(itemIdx,isLast) {
        var me = this,
            el = me.domEl,
            width = me.definition.itwcWidth || me.defaultWidth,
            height = me.definition.itwcHeight || me.defaultHeight,
            direction = me.parentCmp.definition.direction || me.parentCmp.defaultDirection,
            valign = me.parentCmp.definition.valign || 'top',
            halign = me.parentCmp.definition.halign || 'left';

        //Set margins as specified
        el.style.margin = me.margins.map(function(s) { return s + 'px'}).join(' ');

        //Set margins to auto based on alignment of parent
        if(direction == 'vertical') {
            if(width !== 'flex') {
                switch(halign) {
                    case 'left': el.style.marginRight = 'auto'; break;
                    case 'center': el.style.marginRight = 'auto'; el.style.marginLeft = 'auto'; break;
                    case 'right': el.style.marginLeft = 'auto'; break;
                }
            }
            //If this element is the first, maybe also adjust top margin;
            if(itemIdx === 0 && (valign == 'middle' || valign == 'bottom')) {
                el.style.marginTop = 'auto';
            }
            //If this element is the last, maybe also adjust bottom margin;
            if(isLast && (valign == 'middle' || 'top')) {
                el.style.marginBottom = 'auto';
            }
        } else {
            if(height !== 'flex') {
                switch(valign) {
                    case 'top': el.style.marginBottom = 'auto'; break;
                    case 'middle': el.style.marginBottom = 'auto'; el.style.marginTop = 'auto'; break;
                    case 'bottom': el.style.marginTop = 'auto'; break;
                }
            }
            //If this element is the first, maybe also adjust left margin;
            if(itemIdx === 0 && (halign == 'center' || halign == 'right')) {
                el.style.marginLeft = 'auto';
            }
            //If this element is the last, maybe also adjust right margin;
            if(isLast && (halign == 'center' || halign == 'left')) {
                el.style.marginRight = 'auto';
            }
        }
    },
    initItemLayout: function() {
        var me = this,
            el = me.targetEl,
            horizontal = ((me.definition.direction && me.definition.direction) == 'horizontal') || false;

        el.classList.add(horizontal ? 'hcontainer' : 'vcontainer');
    },
    afterAdd: function() {
        var me = this;
        if(me.items && me.items.length) {
            me.items.forEach(function(cmp) {
               cmp.afterAdd();
            });
        }
    },
    afterShow: function() {
        var me = this;
        if(me.items && me.items.length) {
            me.items.forEach(function(cmp) {
               cmp.afterShow();
            });
        }
    },
    afterResize: function() {
        var me = this;
        if(me.items && me.items.length) {
            me.items.forEach(function(cmp) {
                cmp.afterResize();
            });
        }
    },
    beforeDOMRemove: function() {
       var me = this;
        if(me.items && me.items.length) {
            me.items.forEach(function(cmp) {
               cmp.beforeDOMRemove();
            });
        }
    },
    applyUpdate: function(operation,args) {
        var me = this;
        if(me[operation] && typeof me[operation] == 'function') {
            me[operation].apply(me,args);
        } else {
            console.log("Unsupported operation on component",me, operation,args);
        }
    },
    getChildIndex: function() {
        var me = this,
            siblings = me.parentCmp.items,
            num = siblings.length,
            i;
       for(i = 0; i < num; i++) {
            if(siblings[i].domId === me.domId) {
                return i;
            }
       }
       return -1;
    },
    setHotkeys: function(hotkeys) {
        var me = this;

        me.hotkeys = hotkeys;

        if(me.hotkeys.length === 0 && me.hotkeyListener) {
            me.domEl.removeEventListener('keyup',me.hotkeyListener);
            me.hotkeyListener = null;
        } else {
            me.hotkeyListener = me.domEl.addEventListener('keyup',function(e) {
                me.hotkeys.forEach(function(hotkey) {
                    if(e.keyCode === hotkey[0].key) {
                        itwc.controller.sendActionEvent(hotkey[1].taskId,hotkey[1].actionId);
                    }
                });
            });
        }
    },
    setTaskId: function(taskId) {
        this.definition.taskId = taskId;
    }
};
itwc.Container = itwc.extend(itwc.Component,{
    isContainer: true,
    defaultDirection: 'vertical',
    afterItemAdded: null,
    afterItemRemoved: null
});
itwc.Panel = itwc.extend(itwc.Container,{

    panelEl: null,
    headerEl: null,
    titleEl: null,
    closeEl: null,
    menuEl: null,

    initDOMEl: function() {
        this.initPanel();
    },
    initPanel: function() {
        var me = this;

        if(!me.panelEl) {
            me.panelEl = me.domEl;
        }
        me.targetEl = document.createElement('div');
        me.targetEl.classList.add('inner');
        me.panelEl.appendChild(me.targetEl);
    },
    createHeaderEl: function() {
        var me = this;
        me.headerEl = document.createElement('div');
        me.headerEl.classList.add('header');
        me.panelEl.insertBefore(me.headerEl,me.panelEl.childNodes[0]);
    },
    createTitleEl: function() {
        var me = this;
        me.titleEl = document.createElement('span');
        if(me.headerEl.childNodes.length) {
            me.headerEl.insertBefore(me.titleEl,me.headerEl.childNodes[0]);
        } else {
            me.headerEl.appendChild(me.titleEl);
        }
    },
    createCloseEl: function() {
        var me = this;
        me.closeEl = document.createElement('a');
        me.closeEl.innerHTML = 'x';
        me.closeEl.href = '#';
        me.closeEl.classList.add('close');
        me.closeEl.addEventListener('click',function(e) {
            itwc.controller.sendActionEvent(me.definition.closeTaskId,'Close');
            e.preventDefault();
        },me);

        if(me.headerEl.childNodes.length) {
            me.headerEl.insertBefore(me.closeEl,me.headerEl.childNodes[me.headerEl.childNodes.length - 1]);
        } else {
            me.headerEl.appendChild(me.closeEl);
        }
    },
    createMenuEl: function(definition) {
        var me = this;

        me.menu = new itwc.component.itwc_menubar();
        me.menu.definition = definition;
        me.menu.init(definition, me);
        me.menu.render(0,false);
        me.menuEl = me.menu.domEl;

        me.domEl.insertBefore(me.menuEl,me.targetEl);
    },
    setTitle: function(title) {
        var me = this;
        if(title) {
            if(!me.headerEl) { me.createHeaderEl();}
            if(!me.titleEl) { me.createTitleEl();}

            me.titleEl.innerHTML = title;
        } else {
            if(me.titleEl) {
                me.headerEl.removeChild(me.titleEl);
                me.titleEl = null;
            }
            if(me.headerEl && !me.headerEl.childNodes.length) {
                me.domEl.removeChild(me.headerEl);
                me.headerEl = null;
            }
        }
    },
    setCloseTaskId: function(taskId) {
        var me = this;
        me.definition.closeTaskId = taskId;
        if(taskId) {
            if(!me.headerEl) { me.createHeaderEl();}
            if(!me.closeEl) { me.createCloseEl();}
        } else {
            if(me.closeEl) {
                me.headerEl.removeChild(me.closeEl);
                me.closeEl = null;
            }
            if(me.headerEl && !me.headerEl.childNodes.length) {
                me.domEl.removeChild(me.headerEl);
                me.headerEl = null;
            }
        }
    },
    setMenu: function(items) {
        var me = this;

        if(items) {
            if(!me.menuEl) {
                me.createMenuEl({xtype: 'itwc_menubar', items: items});
            }
        } else { //remove
            if(me.menuEl) {
                me.menu.beforeDOMRemove();
                me.domEl.removeChild(me.menuEl);
                me.menuEl = null;
                me.menu = null;
            }
        }
    }
});
itwc.Layer = itwc.extend(itwc.Panel,{
    maximize: false,
    modal: false,
    defaultVpos: 'middle',
    defaultHpos: 'center',

    initDOMEl: function() {
        this.initLayer();
    },
    initLayer: function() {
        var me = this, wrapEl;

        me.domEl.classList.add('layer');
        me.initPanel();
        //If the layer is modal, we need to wrap the element in an additional full size
        //div that masks everything beneath it
        if(me.modal) {
            me.domEl = document.createElement('div');
            me.domEl.classList.add('modal-masker');
            me.domEl.appendChild(me.panelEl);
        }
    },
    initSize: function() {
        var me = this;

        //Layers are put directly in the body and sized by the controller
        if(me.maximize || me.modal) {
            this.domEl.style.width = window.innerWidth;
            this.domEl.style.height = window.innerHeight;
        }

        //Children are size using their iTasks attributes
        me.initItemLayout();
    }
});
//#### CORE UI COMPONENT DEFINITIONS ####//
itwc.component = {};

itwc.component.itwc_menubar = itwc.extend(itwc.Container, {
    defaultDirection: 'horizontal',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        el.classList.add('toolbar');
    },
    initSize: function() {},
    initItemLayout: function() {}
});
itwc.component.itwc_menubutton = itwc.extend(itwc.Component, {
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            linkEl, menuEl;

        el.classList.add('menu-item');
        //Menu button
        linkEl = document.createElement('a');
        linkEl.href = '#';
        linkEl.innerHTML = me.definition.text;
        el.appendChild(linkEl)
        //Menu items
        me.menu = new itwc.component.itwc_menu();
        me.menu.init({xtype: 'itwc_menu', items: me.definition.menu},me);
        me.menu.render(0,false);
        el.appendChild(me.menu.domEl);
    },
    initSize: function() {} //Don't size
});
itwc.component.itwc_menu = itwc.extend(itwc.Container,{
    xtype: 'itwc_menu',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.classList.add('menu');
    },
    initSize: function() {} //Don't size
});
itwc.component.itwc_actionmenuitem = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            iconEl, linkEl;

        el.classList.add('submenu-item');
        me.disabled = me.definition.disabled;

        linkEl = document.createElement('a');
        linkEl.href = '#';
        linkEl.innerHTML = me.definition.text;
        linkEl.addEventListener('click',function(e) {
            if(!me.disabled) {
                itwc.controller.sendActionEvent(me.definition.taskId,me.definition.actionId);
            }
            e.preventDefault();
        });
        el.appendChild(linkEl)
    },
    initSize: function() {} //Don't size
});
itwc.component.itwc_submenuitem = itwc.extend(itwc.Container,{
    initDOMEl: function() {
       var me = this,
           el = me.domEl,
           iconEl, linkEl;
        me.disabled = me.definition.disabled;

        el.classList.add('submenu-item');

        linkEl = document.createElement('a');
        linkEl.href = '#';
        linkEl.innerHTML = me.definition.text;
        el.appendChild(linkEl)
        //Menu items
        me.menu = new itwc.component.itwc_menu();
        me.menu.init({xtype: 'itwc_menu', items: me.definition.menu},me);
        me.menu.render(0,false);
        el.appendChild(me.menu.domEl);
    },
    initSize: function() {} //Don't size
});

itwc.component.itwc_viewport = itwc.extend(itwc.Layer,{
    initDOMEl: function() {
        var me = this;

        me.windows = [];
        me.maximize = true;

        me.domEl.classList.add('viewport');
        me.initLayer();
        if(me.definition.menu) {
            me.setMenu(me.definition.menu);
        }
        itwc.controller.instanceProxies[me.definition.instanceNo] = itwc.controller.remoteProxy;
        itwc.controller.remoteProxy.addInstance(me.definition.instanceNo,me.definition.instanceKey,me);
    },
    beforeDOMRemove: function() {
        var me = this;
        itwc.controller.remoteProxy.removeInstance(me.definition.instanceNo);
    },
    setTitle: function(title) {
        document.title = title || '';
    },
    reset: function() {
    }
});
itwc.component.itwc_window = itwc.extend(itwc.Layer, {
    defaultMargins: [10,10,10,10],
    movable: false,
    initDOMEl: function() {
        var me = this;
        switch(me.definition.windowType) {
            case 'modal':
                me.modal = true;
                me.domEl.classList.add('modal-window');
                break;
            case 'bubble':
                me.domEl.classList.add('notification-bubble');
                break;
            default:
                me.movable = true;
                me.domEl.classList.add('floating-window');
        }
        me.initLayer();
        me.setTitle(me.definition.title);
        me.setCloseTaskId(me.definition.closeTaskId);

        if(me.definition.menu) {
            me.setMenu(me.definition.menu);
        }
        if(me.movable && me.headerEl) {
            me.headerEl.addEventListener('mousedown', me.onStartDrag.bind(me));
            me.headerEl.style.cursor = 'move';
        }
    },
    initSize: function() {
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
        me.initItemLayout();
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
        me.panelEl.style.left = ((left < 0) ? 0 : (left + diffX)) + 'px';
        me.panelEl.style.top = ((top < 0) ? 0 : (top + diffY)) + 'px';

        me.lastX = newX;
        me.lastY = newY;
    },
    onStopDrag: function(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
});
itwc.component.itwc_view_string = itwc.extend(itwc.Component,{
    defaultWidth: 'wrap',
    initDOMEl: function() {
        this.domEl.innerHTML = this.definition.value;
    },
    setValue: function(value) {
        this.domEl.innerHTML = value;
    }
});
itwc.component.itwc_view_html = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        this.domEl.innerHTML = this.definition.value;
        this.domEl.classList.add('view-html');
    },
    setValue: function(value) {
        this.domEl.innerHTML = value;
    }
});
itwc.component.itwc_view_checkbox = itwc.extend(itwc.Component,{
    domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.type = 'checkbox';
        el.checked = me.definition.value;
        el.disabled = true;
    },
    setValue: function(value) {
        this.domEl.checked = value;
    }
});
itwc.component.itwc_view_progress = itwc.extend(itwc.Component,{
    domTag: 'progress',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.innerHTML = me.definition.text;
        el.max = 100;
        if(typeof me.definition.value == 'number') {
            el.value = me.definition.value * 100;
        }
    },
    setValue: function(value) {
        this.domEl.value = value * 100;
    }
});
itwc.component.itwc_view_slider = itwc.extend(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        el.min = me.definition.minValue;
        el.max = me.definition.maxValue;
        el.value = me.definition.value;
        el.disabled = true;
    },
    setValue: function(value) {
        this.domEl.value = value;
    }
});
itwc.component.itwc_view_document = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        var me = this;
        if(me.definition.value) {
            me.setValue(me.definition.value);
        }
    },
    setValue: function(value) {
        var me = this;
        if(value) {
            me.domEl.innerHTML = '<a href="'+value.contentUrl+'" target="_blank">'+value.name+'</a>';
        }
    }
});
itwc.component.itwc_edit_string = itwc.extend(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value, true);
        });
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
            this.domEl.value = value || '';
        }
    }
});
itwc.component.itwc_edit_password = itwc.extend(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'password';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value, true);
        });
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
            this.domEl.value = value || '';
        }
    }
});
itwc.component.itwc_edit_note= itwc.extend(itwc.Component,{
    domTag: 'textarea',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.innerHTML = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value, true);
        });
    },
    setEditorValue: function(value) {
        var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;

		if(receivedNo > sentNo) {
            this.domEl.value = value || '';
        }
    }
});
itwc.component.itwc_edit_checkbox = itwc.extend(itwc.Component,{
    domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'checkbox';
        el.checked = me.definition.value;

        el.addEventListener('click',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.checked,false);
        });
    },
    setValue: function(value) {
        this.domEl.checked = value;
    },
    setEditorValue: function(value) {
		this.domEl.checked = value || false;
    }
});
itwc.component.itwc_edit_number = itwc.extend(itwc.Component,{
    allowDecimal: false,
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keypress',function(e) {
            if(me.invalidKey(e.which)) {
                e.stopPropagation();
                e.preventDefault();
            }
        });
        el.addEventListener('keyup',function(e) {
            var value;
            if(me.invalidKey(e.which)) {
                return;
            }
            if(e.target.value === "") {
                value = null;
            } else if(me.invalidValue(e.target.value)) {
                value = e.target.value;
            } else {
                value = me.allowDecimal ? parseFloat(e.target.value) : (e.target.value | 0);
            }
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,value,true);
        });
    },
    invalidKey: function(charCode) {
        return !(charCode < 32 || (charCode > 47 && charCode < 58) || charCode == 45 || (this.allowDecimal && charCode == 46));
    },
    invalidValue: function(value) {
        var me = this, i;
        for(i = 0; i < value.length; i++) {
            if(me.invalidKey(value.charCodeAt(i))) {
                return true;
            }
        }
        return false;
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
			this.domEl.value = value;
        }
    }
});
itwc.component.itwc_edit_int = itwc.extend(itwc.component.itwc_edit_number,{
    allowDecimal: false
});
itwc.component.itwc_edit_decimal = itwc.extend(itwc.component.itwc_edit_number,{
    allowDecimal: true
});
itwc.component.itwc_edit_date = itwc.extend(itwc.Component,{
    domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
			this.domEl.value = value || '';
        }
    }
});
itwc.component.itwc_edit_time = itwc.extend(itwc.Component,{
    domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
			this.domEl.value = value || '';
        }
    }
});
itwc.component.itwc_edit_datetime = itwc.extend(itwc.Component,{
    domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.definition.value ? me.definition.value : '';
        el.addEventListener('keyup',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    },
    setEditorValue: function(value) {
		var instanceNo = this.definition.taskId.split("-")[0],
            receivedNo = itwc.controller.instanceProxies[instanceNo].lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
			this.domEl.value = value || '';
        }
    }
});

itwc.component.itwc_edit_slider = itwc.extend(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        el.min = me.definition.minValue;
        el.max = me.definition.maxValue;
        el.value = me.definition.value;

        el.addEventListener('change',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId, (e.target.value | 0),true);
        });
    },
    setValue: function(value) {
        this.domEl.value = value;
    },
    setEditorValue: function(value) {
		this.domEl.value = value;
    }
});
itwc.component.itwc_edit_document = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.classList.add('edit-document');

        //Create a hidden file selector
        me.fileEl = document.createElement('input');
        me.fileEl.type = "file";
        me.fileEl.style.display = "none";
        me.fileEl.addEventListener('change',me.onFileSelect.bind(me));
        el.appendChild(me.fileEl);

        me.labelEl = document.createElement('span');
        el.appendChild(me.labelEl);

        me.actionEl = document.createElement('a');
        me.actionEl.href = "#";
        me.actionEl.addEventListener('click',me.onAction.bind(me));
        el.appendChild(me.actionEl);

        me.xhr = null;
        me.value = me.definition.value || null;
        me.showValue();
    },
    showUploading: function(progress) {
        this.labelEl.innerHTML = "Uploading... " + progress + "%";
        this.actionEl.innerHTML = "Cancel";
    },
    showValue: function() {
        var me = this;
        if(me.value !== null) {
            me.labelEl.innerHTML = '<a href="'+me.value.contentUrl+'" target="_blank">'+me.value.name+'</a>';
            me.actionEl.innerHTML = 'Clear';
        } else {
            me.labelEl.innerHTML = 'No file selected';
            me.actionEl.innerHTML = 'Select';
        }
    },
    onAction: function(e) {
        var me = this;
        e.preventDefault();

        if(me.xhr != null) { //Cancel
            me.xhr.abort();
            me.xhr = null;
            me.showValue();
            return;
        }
        if(me.value != null) { //Clear;
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,null,false);
            me.value = null;
            me.showValue();
        } else { //Select
            me.fileEl.click();
        }
    },
    onFileSelect: function() {
        var me = this,
            fd;

        //Create uploader
        me.xhr = new XMLHttpRequest();
        me.xhr.upload.addEventListener('progress',function(e) {
            me.showUploading(Math.round((e.loaded * 100) / e.total));
        });
        me.xhr.onreadystatechange = me.onUploadStateChange.bind(me);
        me.xhr.open('POST','?upload',true);
        //Add file to upload data
        fd = new FormData();
        fd.append('upload',me.fileEl.files[0]);
        me.xhr.send(fd);
    },
    onUploadStateChange: function(e) {
        var me = this, rsp;

        if (me.xhr.readyState == 4 && me.xhr.status == 200) {
            //Upload ready
            rsp = JSON.parse(me.xhr.responseText);

            //Switch to value state
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,rsp[0],false);
            me.xhr = null;
            me.value = rsp[0];
            me.showValue();
        }
    },
    setEditorValue: function(value) {
        if(me.xhr != null) {
            me.xhr.abort();
            me.xhr = null;
        }
        me.value = value; 
        me.showValue();
    }
});
itwc.component.itwc_edit_editlet = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl, tmp;

		me.dataVersion = 1;
		me.diffContinuations = {};
		me.diffQueue = [];
		me.waitingResponse = false;
		
        me.htmlId = "editlet-" + me.definition.taskId + "-" + me.definition.editorId;
		itwc.controller.editlets[me.htmlId] = me;

        el.innerHTML = me.definition.html;

        //Prepare javascript
        if(me.definition.script != null && me.definition.script != "" && !sapldebug) {
            evalScript(me.definition.script);
			delete me.definition.script;
			_dynamic_hijack();					
        }
        if(me.definition.appDiff != null) {
            eval("tmp = " + me.definition.appDiff + ";");
            me.appDiff = tmp;
            delete me.definition.appDiff;
        }
        if(me.definition.initDiff != null) {
            eval("tmp = " + me.definition.initDiff + ";");
            me.initDiff = tmp;
            delete me.definition.initDiff;
        }
        if(me.definition.initClient != null) {
			eval("tmp = " + me.definition.initClient + ";");
			me.initClient = tmp;			
	        delete me.definition.initClient;
        }
	},
    addManagedListener: function(obj,eventname,handler,thisObj) {
        var me = this;
        //TEMPORARY FOR EXTJS STUFF IN EXTJS STYLE
        if(eventname == "afterlayout") {
            me.eventAfterLayout = handler;
        }
    },
    afterAdd: function() {
        var me = this;
		
		var ys = Sapl.feval([me.initClient,[me.htmlId,"JSWorld"]]);
		
		//Strict evaluation of all the fields in the result tuple
		Sapl.feval(ys[2]);
		Sapl.feval(ys[3]);

		// save state return by appDiff
		me.value = ys[2];		
			
		// Attach event handlers
		if(me.definition.events){
			for (var i=0; i<me.definition.events.length; ++i){
				var elname = me.definition.events[i][0];
				var eventName = me.definition.events[i][1];
				eval("tmp = " + me.definition.events[i][2] + ";");
				var expr = tmp;
							
				var el = document.getElementById(elname);
				el.addEventListener(eventName, me.eventHandler(true,expr));
			}
		}		
		
		if(me.initDiff != null && me.initDiff[0]==1) /* Just */ {
			var ys = Sapl.feval([me.appDiff,[me.htmlId,me.initDiff[2],me.value,"JSWorld"]]);

			//Strict evaluation of all the fields in the result tuple
			Sapl.feval(ys[2]);
			Sapl.feval(ys[3]);

			// save state return by appDiff
			me.value = ys[2];	
		}	

        //TEMPORARY FOR EXTJS STUFF
        if(me.eventAfterLayout) {
            me.eventAfterLayout.apply(me,["dummy event"]);
        }			
    },
    afterShow: function() {},
	// Creating a closure
	eventHandler: function(jsevent,expr){
		var me = this;
		
		var h = function(dummy){

			var event = dummy;
		
			if(jsevent){
				event = [0,"ARRAY"];
				for(var i=0; i<arguments.length; i++) event.push(___wrapJS(arguments[i]));
			}
			
			var ys = Sapl.feval([expr,[me.htmlId,event,me.value,"JSWorld"]]);

            if(typeof ys == 'undefined') {
                console.warn('eventHandler: evaluating expression yielded undefined',expr);
            }
			//Strict evaluation of all the fields in the result tuple
			me.value = Sapl.feval(ys[2]);
			diffing = Sapl.feval(ys[3]);
			Sapl.feval(ys[4]); // world
			
			if(diffing[0] == 1){ // Diff

				if(me.waitingResponse){

					console.log("queue");
				
					me.diffQueue.push(diffing);
				
				}else{

					var diff = Sapl.toJS(diffing[2]);
					var callback = Sapl.heval(diffing[3]);			
					var diffId = Date.now() | 0;

					me.diffContinuations[diffId] = callback;
					me.waitingResponse = true;
		
					console.log("send", diffId);
		
					itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[me.dataVersion, diffId, diff],false);			
				
				}
			}
			
		};
		return h;
	},
	diffCallback: function(conflict, diffId){
        var me = this;
		
		console.log("callback", diffId);
		
		var callback = me.diffContinuations[diffId];
		delete me.diffContinuations[diffId];	
		
		var ys = Sapl.feval([callback,[conflict,me.value,"JSWorld"]]);
		
		//Strict evaluation of all the fields in the result tuple
		me.value = Sapl.feval(ys[2]);
		diffing = Sapl.feval(ys[3]);
		Sapl.feval(ys[4]); // world
		
		if(diffing[0] != 1 && me.diffQueue.length > 0) {console.log("dequeue");diffing = me.diffQueue.shift();}
		
		if(diffing[0] == 1){ // Diff

/*		
  var date = new Date();
  var curDate = null;
  do { curDate = new Date(); }
  while(curDate-date < 40);
*/		
		
			var diff = Sapl.toJS(diffing[2]);
			var callback = Sapl.feval(diffing[3]);			
			var diffId = Date.now() | 0;

			me.diffContinuations[diffId] = callback;

			console.log("send", diffId);
			
			itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[me.dataVersion, diffId, diff],false);			
		}
		else
		{
			me.waitingResponse = false;
		}
	},
    rollbackDiff: function(diffId) {
		this.diffCallback(true, diffId);
    },
    commitDiff: function(diffId) {
		this.diffCallback(false, diffId);
	},		
    applyDiff: function(dataVersion,saplDiff,extraJS) {

        var me = this,
            tmp;
			
		me.dataVersion = dataVersion;
					
        if(extraJS != "") {
            evalScript(extraJS);
        }
        eval("tmp = " + saplDiff + ";");

		var ys = Sapl.feval([me.appDiff,[me.htmlId,tmp[2],me.value,"JSWorld"]]);

		//Strict evaluation of all the fields in the result tuple
		Sapl.feval(ys[2]);
		Sapl.feval(ys[3]);

		// save state return by appDiff
		me.value = ys[2];				
    },
	jsFromSaplJSONNode: function (sapl) {
		switch(sapl[0]) {
			case 0:	return null;
			case 1: return sapl[2];
			case 2: return sapl[2];
			case 3: return sapl[2];
			case 4: return sapl[2];
			case 5: return this.jsFromList(sapl[2]);
			case 6:
				return this.jsFromFieldList({},sapl[2]);			
		}
	},
	jsFromList: function(sapl) {
		if(sapl[0] == 0) {
			return ([this.jsFromSaplJSONNode(sapl[2])]).concat(this.jsFromList(sapl[3]));
		} else {
			return [];
		}
	},
	jsFromFieldList: function (fields,sapl) {
		
		if(sapl[0] == 0) {
			fields = this.jsFromField(fields,sapl[2]);
			fields = this.jsFromFieldList(fields,sapl[3]);
		}
		return fields;
	},
	jsFromField: function (fields,sapl) {
		fields[sapl[2]] = this.jsFromSaplJSONNode(sapl[3]);
		return fields;
	}
});
itwc.component.itwc_tasklet = itwc.extend(itwc.Container,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl, tmp, proxy;

        me.windows = [];

		if(me.definition.html) {
			el.innerHTML = me.definition.html;
        }
        // Prepare javascript
        if(me.definition.script != null && me.definition.script != "" && !sapldebug) {
            evalScript(me.definition.script);
			delete me.definition.script;
			_dynamic_hijack();			
		}
		
		// Prepare state
		eval("var tmp = eval(" + me.definition.st + ");");
		me.definition.st = Sapl.feval(tmp);
		itwc.controller.tasklets[me.definition.taskId] = me;
		
		if(me.definition.resultFunc != null){
			eval("tmp = " + me.definition.resultFunc + ";");
			me.definition.resultFunc = tmp;
			me.definition.lastResult = Sapl.toJS(Sapl.feval([me.definition.resultFunc,[me.definition.st]]));
		}
		
		if(me.definition.controllerFunc != null){
		
			// Prepare IWorld
			if(!_iworld){
				var url = "//" + document.location.host;
				_iworld = Sapl.fapp(__iTasks_Framework_Client_RunOnClient_createClientIWorld, [url, me.definition.instanceNo]);
			}				
		
			console.time('controllerWrapper timer: eval');
				
			eval("tmp = " + me.definition.controllerFunc + ";");
			me.definition.controllerFunc = tmp;

            //Create task instance proxy
            proxy = new itwc.taskletInstanceProxy();
            proxy.init(itwc.controller);
            proxy.setRootNode(me);

            itwc.controller.instanceProxies[me.definition.instanceNo] = proxy;
						
			var ret = Sapl.fapp(me.definition.controllerFunc, [me.definition.taskId, me.definition.st, __Data_Maybe_Nothing, __Data_Maybe_Nothing, __Data_Maybe_Nothing, _iworld]);
			me.definition.st = Sapl.feval(ret[3]);
			_iworld = Sapl.feval(ret[4]);
			
			var ui = Sapl.toJS(Sapl.feval(ret[2]));
			
			console.timeEnd('controllerWrapper timer: eval');
			
			console.time('controllerWrapper timer: apply UI');
			itwc.controller.updateUI({instance: me.definition.instanceNo, updates: JSON.parse(ui)}, me);
			console.timeEnd('controllerWrapper timer: apply UI');

			// Start background process on _iworld
			if(!_itask_background_interval){
				window.setInterval(__itask_background_process,200);
			}
		}
	},
    afterAdd: function() {
		var me = this;
		
		// Attach event handlers
		if(me.definition.events){
			for (var i=0; i<me.definition.events.length; ++i){
				var elname = me.definition.events[i][0];
				var eventName = me.definition.events[i][1];
				var expr = me.definition.events[i][2];
							
				if(elname == "tasklet"){
					if(eventName == "init"){
						(me.eventHandler(expr))(me);
					}
				}else{
					var el = document.getElementById(elname);
					el.addEventListener(eventName, me.eventHandler(expr));
				}
			}
		}
	},
	// Creating a closure
	eventHandler: function(expr){
		
		var h = function(event){
			eval("var tmp = " + expr + ";");
			Sapl.fapp(tmp,[arguments]);
		};

		return h;
	}
});
itwc.ButtonComponent = itwc.extend(itwc.Component,{
    domTag: 'a',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        el.classList.add('button');
        el.href = '#';

        if(me.definition.iconCls) {
            me.icon = document.createElement('div');
            me.icon.classList.add('button-icon');
            me.icon.classList.add(me.definition.iconCls);
            el.appendChild(me.icon);
        }
        me.disabled = me.definition.disabled || false;
        if(me.disabled) {
            el.classList.add('button-disabled');
        }
        if(me.definition.text) {
            me.label = document.createElement('div');
            me.label.innerHTML = me.definition.text;
            me.label.classList.add('button-label');
            el.appendChild(me.label);
        }
        el.addEventListener('click',function(e) {
            if(!me.disabled) {
                me.onClick(e);
            }
            e.preventDefault();
            return false;
        });
    },
    setDisabled: function(disabled) {
        var me = this,
            el = me.domEl;
        me.disabled = disabled;
        el.classList[disabled ? 'add':'remove']('button-disabled');
    },
    setTaskId: function(taskId) {
        this.definition.taskId = taskId;
    },
    setActionId: function(actionId) {
        this.definition.actionId = actionId;
    },
    setText: function(text) {
        this.label.innerHTML = text;
    },
    setIconCls: function (iconCls) {
        var me = this;

        if(iconCls == null) {
            if(me.definition.iconCls) {
                me.domEl.removeChild(me.icon);
            }
            me.definition.iconCls = null;
        } else { //Set icon
            if(me.definition.iconCls) {
                me.icon.classList.remove(me.definition.iconCls);
                me.definition.iconCls = iconCls;
                me.icon.classList.add(me.definition.iconCls);
            } else {
                me.definition.iconCls = iconCls;
                me.icon = document.createElement('div');
                me.icon.classList.add('button-icon');
                me.icon.classList.add(me.definition.iconCls);
                me.domEl.insertBefore(me.icon,me.label);
            }
        }
    }
});
itwc.component.itwc_actionbutton = itwc.extend(itwc.ButtonComponent,{
    onClick: function (e) {
        var me = this;
        itwc.controller.sendActionEvent(me.definition.taskId,me.definition.actionId);
    }
});
itwc.component.itwc_editbutton = itwc.extend(itwc.ButtonComponent,{
    onClick: function (e) {
        var me = this;
        itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,me.definition.value,false);
    },
    setEditorValue: function(value) {
        var me = this;
        me.definition.value = value || '';
    }
});
itwc.component.itwc_clientbutton = itwc.extend(itwc.ButtonComponent,{
    onClick: function(e) {
        var me = this;
        itwc.controller[me.definition.actionId]();
    }
});

itwc.component.itwc_icon= itwc.extend(itwc.Component,{
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.classList.add('icon');
        el.classList.add(me.definition.iconCls);
        me.currentIcon = me.definition.iconCls;

        if(me.definition.tooltip) {
            el.setAttribute('tooltip',me.definition.tooltip);
        }
    },
    setIconCls: function(iconCls) {
        var me = this,
            el = me.domEl;
        el.classList.remove(me.currentIcon);
        me.currentIcon = iconCls;
        el.classList.add(me.currentIcon);
    },
    setTooltip: function(tooltip) {
        var me = this,
            el = me.domEl;
        el.setAttribute('tooltip',tooltip);
    }
});
itwc.component.itwc_label = itwc.extend(itwc.Container,{
    domTag: 'label',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.innerHTML = me.definition.text;
    }
});
itwc.component.itwc_container = itwc.extend(itwc.Container,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        if(me.definition.baseCls) {
            el.classList.add(me.definition.baseCls);
        }
    }
});
itwc.component.itwc_fieldset = itwc.extend(itwc.Container,{
    defaultHeight: 'flex',
    initDOMEl: function() {
        var me = this,
            el = me.domEl, header;

        if(me.definition.baseCls) {
            el.classList.add(me.definition.baseCls);
        }
        el.classList.add('fieldset');
        if(me.definition.title) {
            header = document.createElement('div');
            header.innerHTML = me.definition.title;
            header.classList.add('fieldset-header');

            el.appendChild(header);
        }
        me.targetEl = document.createElement('div');
        me.targetEl.classList.add('inner');
        el.appendChild(me.targetEl);
    },
    setTitle: function(title) {
        this.domEl.childNodes[0].innerHTML = title;
    }
});
itwc.component.itwc_panel = itwc.extend(itwc.Panel,{
    defaultHeight: 'flex',
    hasTitle: false,
    initDOMEl: function() {
        var me = this,
            el = me.domEl, header;

        me.initPanel();

        el.classList.add('panel');

        if(me.definition.baseCls) {
            el.classList.add(me.definition.baseCls);
        }
        if(me.definition.frame) {
            el.classList.add('framed');
        }
        me.setTitle(me.definition.title);
        if(me.definition.menu) {
            me.setMenu(me.definition.menu);
        }
    }
});
itwc.component.itwc_tabset = itwc.extend(itwc.Container,{
    isContainer: false, //Don't size as container
    defaultWidth: 'flex',
    defaultHeight: 'flex',
    activeTab: 0,

    initDOMEl: function() {
        var me = this,
            el = me.domEl, bar;

        me.domEl.classList.add('tabset');
        me.tabBar = document.createElement('ul');
        me.tabBar.classList.add('tabbar');
        me.domEl.appendChild(me.tabBar);

        me.targetEl = document.createElement('div');
        me.targetEl.classList.add('tabitems');
        me.domEl.appendChild(me.targetEl);
    },
    afterItemAdded: function(itemIdx,itemCmp) {
        var me = this,
            tab,icon,label,closeLink;

        //Add a tab
        tab = document.createElement('li');

        label = document.createElement('a');
        label.innerHTML = '<span>'+itemCmp.definition.title+'</span>';
        label.href = '#';
        if(itemCmp.definition.focusTaskId) {
            label.addEventListener('click',function(e) {
                itwc.controller.sendFocusEvent(itemCmp.definition.focusTaskId);
                e.preventDefault();
            },me);
        }
        if(itemCmp.definition.iconCls) {
            icon = document.createElement('div');
            icon.classList.add('tabicon');
            icon.classList.add(itemCmp.definition.iconCls);
            label.insertBefore(icon,label.childNodes[0]);
        }
        tab.appendChild(label);

        if(itemCmp.definition.closeTaskId) {
            closeLink = document.createElement('a');
            closeLink.innerHTML = 'x';
            closeLink.href = '#';
            closeLink.classList.add('tabclose');
            closeLink.addEventListener('click',function(e) {
                itwc.controller.sendActionEvent(itemCmp.definition.closeTaskId,'Close');
                e.preventDefault();
            },me);

            tab.appendChild(closeLink);
        }
        if(itemIdx == me.activeTab) {
            tab.classList.add('selected');
        }
        if(itemIdx === me.tabBar.childNodes.length) {
            me.tabBar.appendChild(tab);
        } else {
            me.tabBar.insertBefore(tab,me.tabBar.childNodes[itemIdx]);
        }
    },
    afterItemRemoved: function(itemIdx) {
        var me = this;
        me.tabBar.removeChild(me.tabBar.childNodes[itemIdx]);
    },
    afterShow: function() {
        var me = this;
        //Only propagate to active tab
        if(me.items[me.activeTab]) {
            me.items[me.activeTab].afterShow();
        }
    },
    setActiveTab: function(activeTab) {
        var me = this;

        if(me.items[me.activeTab]) {
            me.items[me.activeTab].domEl.classList.remove('selected');
            me.tabBar.childNodes[me.activeTab].classList.remove('selected');
        }
        me.activeTab = activeTab || 0;

        if(me.items[me.activeTab]) {
            me.items[me.activeTab].domEl.classList.add('selected');
            me.tabBar.childNodes[me.activeTab].classList.add('selected');
            //Let the tab know it is being shown again
            me.items[me.activeTab].afterShow();
        }
    }
});
itwc.component.itwc_tabitem = itwc.extend(itwc.Panel,{

    initDOMEl: function(itemIdx) {
        var me = this,
            el = me.domEl;

        me.initPanel();
        el.classList.add('tabitem');
        if(itemIdx === me.parentCmp.activeTab) {
            el.classList.add('selected');
        }
        if(me.definition.menu) {
            me.setMenu(me.definition.menu);
        }
    },
    initSize: function() {
        //Size is managed with css by tabset container
        //but we need to set alignment and direction for the items
        //in the tab
        this.initItemLayout();
    },
    setTitle: function(title) {
        var me = this, label;
        me.definition.title = title;

        //Update label of tab in DOM
        label = me.parentCmp.tabBar.childNodes[me.getChildIndex()].childNodes[0];
        label.childNodes[label.childNodes.length - 1].innerHTML = title; //Title is always the last element of a label
    },
    setIconCls: function(iconCls) {
        var me = this, label, icon;

        //Update label of tab in DOM
        label = me.parentCmp.tabBar.childNodes[me.getChildIndex()].childNodes[0];

        if(iconCls.length == 0) { //Remove icon if it was there
            if(label.childNodes.length > 1) {
                label.removeChild(label.childNodes[0]);
            }
        } else {
            if(label.childNodes.length > 1) { //Update existing icon
                icon = label.childNodes[0];
                icon.classList.remove(me.definition.iconCls);
                icon.classList.add(iconCls);
            } else {
                icon = document.createElement('div');
                icon.classList.add('tabicon');
                icon.classList.add(iconCls);
                label.insertBefore(icon,label.childNodes[0]);
            }
        }
        me.definition.iconCls = iconCls;
    },
    setFocusTaskId: function(focusTaskId) {
        var me = this;
        me.definition.focusTaskId = focusTaskId.length ? focusTaskId[0] : null;
    },
    setCloseTaskId: function(closeTaskId) {
        var me = this;
        me.definition.closeTaskId = closeTaskId.length ? closeTaskId[0] : null;
    }
});
itwc.component.itwc_splitter = itwc.extend(itwc.Component, {
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.direction = (me.parentCmp && me.parentCmp.definition.direction) || 'vertical';
        me.vertical = me.direction == 'vertical';
        el.classList.add(me.vertical ? 'vsplitter' : 'hsplitter');
    },
    initSize: function() {
    },
    initMargins: function() {
    },
    afterAdd: function() {
        var me = this,
            el = me.domEl;

        me.prev = el.previousSibling;
        me.next = el.nextSibling;

        el.addEventListener('mousedown', me.onStartDrag.bind(me));
    },
    onStartDrag: function(e) {
        var me = this;

        e.preventDefault();
        me.lastPos = me.vertical ? e.clientY : e.clientX;
        me.onDragging_ = me.onDragging.bind(me);
        me.onStopDrag_ = me.onStopDrag.bind(me);
        window.addEventListener('mousemove', me.onDragging_);
        window.addEventListener('mouseup', me.onStopDrag_);
    },
    onDragging: function(e) {
        var me = this,
            vertical = me.vertical,
            newPos = vertical ? e.clientY : e.clientX,
            sizePrev, sizeNext, sizeDiff = newPos - me.lastPos;

        sizePrev = document.defaultView.getComputedStyle(me.prev,'').getPropertyValue(vertical ? 'height':'width');
        sizeNext = document.defaultView.getComputedStyle(me.next,'').getPropertyValue(vertical ? 'height':'width');
        sizePrev = ((sizePrev,10) + sizeDiff) | 0;
        sizeNext = ((sizeNext,10) - sizeDiff) | 0;
        me.prev.style[vertical ? 'height' : 'width'] = sizePrev + 'px';
        me.next.style[vertical ? 'height' : 'width'] = sizeNext + 'px';

        me.lastPos = newPos;
    },
    onStopDrag: function(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
});
itwc.component.itwc_choice_dropdown = itwc.extend(itwc.Component,{
    defaultWidth: 'wrap',
    domTag: 'select',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            value = me.definition.value[0],
            option;

        option = document.createElement('option');
        option.innerHTML = "Select...";
        option.value = -1;
        el.appendChild(option);

        me.definition.options.forEach(function(label,index) {
            option = document.createElement('option');
            option.value = index;
            option.innerHTML = label;
            if(index === value) {
                option.selected = true;
            }
            el.appendChild(option);
        },me);

        el.addEventListener('change',function(e) {
            var value = e.target.value | 0;
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,value == -1 ? null : value,false);
        });
    },
    setValue: function(selection) {
        var me = this,
            value;
        if(selection.length == 0) {
            value = -1;
        } else {
            value = selection[0];
        }
        me.domEl.value = value;
    }
});
itwc.component.itwc_choice_radiogroup = itwc.extend(itwc.Component,{
    domTag: 'ul',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            inputName = "choice-" + me.definition.taskId + "-" + me.definition.editorId,
            value = me.definition.value.length ? me.definition.value[0] : null;

        el.classList.add('choice-radiogroup');

        me.definition.options.forEach(function(option,idx) {
            var liEl,inputEl,labelEl;
            liEl = document.createElement('li');
            inputEl = document.createElement('input');
            inputEl.type = 'radio';
            inputEl.value = idx;
            inputEl.name = inputName;
            inputEl.id = inputName + "-option-" + idx;
            if(idx === value) {
                inputEl.checked = true;
            }
            inputEl.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,idx,false);
            });
            liEl.appendChild(inputEl);

            labelEl = document.createElement('label');
            labelEl.setAttribute('for',inputName + "-option-" + idx);
            labelEl.innerHTML = option;
            liEl.appendChild(labelEl);

            el.appendChild(liEl);
        });
    }
});
itwc.component.itwc_choice_list = itwc.extend(itwc.Component,{
    domTag: 'div',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            value = me.definition.value.length ? me.definition.value[0] : null;

        el.classList.add('choice-list');

        me.definition.options.forEach(function(option,idx) {
            var optionEl;
            optionEl = document.createElement('div');
            optionEl.classList.add('choice-list-option');
            if(idx === value) {
                optionEl.classList.add('selected');
            }
            optionEl.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,idx,false);
            });
            optionEl.innerHTML = option;

            el.appendChild(optionEl);
        });
    }
});
itwc.component.itwc_choice_checkboxgroup = itwc.extend(itwc.Component,{
    domTag: 'ul',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            inputName = "choice-" + me.definition.taskId + "-" + me.definition.editorId,
            value = me.definition.value || [];

        el.classList.add('choice-checkboxgroup');
        me.definition.options.forEach(function(option,idx) {
            var liEl,inputEl,labelEl;
            liEl = document.createElement('li');
            inputEl = document.createElement('input');
            inputEl.type = 'checkbox';
            inputEl.value = idx;
            inputEl.id = inputName + "-option-" + idx;
            if(value.indexOf(idx) !== -1) {
                inputEl.checked = true;
            }
            inputEl.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[idx,e.target.checked],false);
            });
            liEl.appendChild(inputEl);

            labelEl = document.createElement('label');
            labelEl.setAttribute('for',inputName + "-option-" + idx);
            labelEl.innerHTML = option;
            liEl.appendChild(labelEl);

            el.appendChild(liEl);
        });
    }
});
itwc.component.itwc_choice_tree = itwc.extend(itwc.Component,{
    defaultHeight: 'flex',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            rootNodeId = me.definition.taskId+ "-" + me.definition.editorId + "-node",
            rootNode,node;

        el.classList.add('choicetree');

        rootNode = document.createElement('ol');

        //Create a table for quick access
        me.selection = me.definition.value || [];
        me.nodes = [];

        me.definition.options.forEach(function(option,idx) {
            me.addNode(option,rootNode,rootNodeId,idx);
        },me);

        me.selection.forEach(function(idx) {
            me.nodes[idx].classList.add('selected');
        });
        el.appendChild(rootNode);
    },
    addNode: function(option,parentNode,rootNodeId,idx) {
        var me = this,
            node,nodeId,label,childExpand,childOl;

        nodeId = rootNodeId + "-"+ idx;
        node = document.createElement('li');
        node.id = nodeId;
        if(option.leaf) {
            node.classList.add('leaf');
        }
        label = document.createElement('label');
        label.id = nodeId + "-l";

        if(option.iconCls) {
            label.classList.add(option.iconCls);
        } else {
            label.classList.add('default-' + (option.leaf ? 'leaf' : 'folder'));
        }
        label.innerHTML = option.text;
        label.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,["sel",option.value,true],false);
        },me);

        if(me.definition.doubleClickAction) {
            label.addEventListener('dblclick',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,["sel",option.value,true],false);
                itwc.controller.sendActionEvent(me.definition.doubleClickAction[0],me.definition.doubleClickAction[1]);

                e.stopPropagation();
                e.preventDefault();
            });
        }
        node.appendChild(label);

        if(option.children && option.children.length) {
            childExpand = document.createElement('input');
            childExpand.type = "checkbox"
            childExpand.id = nodeId + "-e";

            if(option.expanded) {
                childExpand.checked = true;
            }
            childExpand.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,["exp",option.value,childExpand.checked],false);
            },me);

            node.appendChild(childExpand);
            childOl = document.createElement('ol');
            option.children.forEach(function(option,childIdx) {
                me.addNode(option,childOl,nodeId,childIdx);
            },me);
            node.appendChild(childOl);
        }
        parentNode.appendChild(node);
        //Keep reference
        me.nodes[option.value] = node;
    },
    setValue: function(value) {
        var me = this;
        me.selection.forEach(function(idx) {
            me.nodes[idx].classList.remove('selected');
        });
        me.selection = value;
        me.selection.forEach(function(idx) {
            me.nodes[idx].classList.add('selected');
        });
    }
});
itwc.component.itwc_choice_grid = itwc.extend(itwc.Component,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            headerEl,bodyEl,rowEl,cellEl;

        el.classList.add('choicegrid');

        //Create header
        headerEl = me.headerEl = document.createElement('div');
        headerEl.classList.add('choicegrid-header');
        me.definition.columns.forEach(function(column) {
            cellEl = document.createElement('div');
            cellEl.innerHTML = column;
            headerEl.appendChild(cellEl);
        });
        el.appendChild(headerEl);

        //Create body
        bodyEl = me.bodyEl = document.createElement('div');
        bodyEl.classList.add('choicegrid-body');
        me.definition.options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');
            rowEl.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[rowIdx],false);
            },me);
            if(me.definition.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
                    itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[rowIdx],false);
                    itwc.controller.sendActionEvent(me.definition.doubleClickAction[0],me.definition.doubleClickAction[1]);

                    e.stopPropagation();
                    e.preventDefault();
                },me);
            }
            option.forEach(function(cell) {
                cellEl = document.createElement('div');
                cellEl.innerHTML = cell;
                rowEl.appendChild(cellEl);
            });
            bodyEl.appendChild(rowEl);
        });
        //Indicate selection
        if(me.definition.value.length) {
            me.definition.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add('selected');
            });
        }
        el.appendChild(bodyEl);
    },
    setValue: function(value) {
        var me = this,
            bodyEl = me.bodyEl;

        //Remove old selection
        me.definition.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.remove('selected');
        });
        //Indicate new selection
        me.definition.value = value;
        me.definition.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add('selected');
        });
    }
});

itwc.component.itwc_embedding = itwc.extend(itwc.Panel, {
    initDOMEl: function() {
        var me = this;

        me.windows = [];
        me.domEl.classList.add('panel');
        me.initPanel();

        itwc.controller.instanceProxies[me.definition.instanceNo] = itwc.controller.remoteProxy;
        itwc.controller.remoteProxy.addInstance(me.definition.instanceNo,me.definition.instanceKey,me);
        itwc.controller.remoteProxy.sendResetEvent(me.definition.instanceNo);
    },
    beforeDOMRemove: function() {
        var me = this;
        itwc.controller.remoteProxy.removeInstance(me.definition.instanceNo);
    },
    reset: function() {
        var me = this, i;
        for(i = me.targetEl.childNodes.length - 1; i >= 0; i--) {
            me.targetEl.removeChild(me.targetEl.childNodes[i]);
        }
    }
});
//#### CENTRAL CONTROLLER ####//

//Proxy that relays events to a local object or
//web service that processes events for a task intstance
itwc.taskInstanceProxy = function() {
    var me = this;

    me.rootNode = null;
};
itwc.taskInstanceProxy.prototype = {
    init: function(controller) {},
    sendResetEvent: function(instanceNo) {},
    sendEditEvent: function(taskId, editorId, value, replace) {},
    sendActionEvent: function(taskId, actionId) {},
    sendFocusEvent: function(taskId) {}
}
itwc.remoteInstanceProxy = itwc.extend(itwc.taskInstanceProxy,{

    init: function(controller) {
        var me = this,
            urlSplit;
        me.controller = controller;
        me.nextSendEventNo = 0;
        me.lastReceivedEventNo = 0;
        me.taskEvents = [];
        me.updateSource = null;
        me.flushingTaskEvents = false;
        me.instances = {};

        //Check url parameters
        if((urlSplit = window.location.toString().split('?')).length == 2) {
            me.urlParameters = '?'+urlSplit[1]
        } else {
            me.urlParameters = '';
        }
    },
    addInstance: function (instanceNo,instanceKey,rootNode) {
        var me = this;
        me.instances[instanceNo] = {instanceKey: instanceKey, rootNode: rootNode};
        me.restartUIEventSource();
    },
    removeInstance: function (instanceNo) {
        var me = this;
        delete me.instances[instanceNo];
        //Also remove pending events
        me.taskEvents = me.taskEvents.filter(function(e) {return (e[0] != instanceNo);});
        me.restartUIEventSource();
    },
    queueTaskEvent: function (instanceNo,eventData,replaceId) {
        var me = this,
            eventNo, queueLen, i;

        if(replaceId) { //If a replaceId is given see if it is already queued and update the queued event
            queueLen = me.taskEvents.length;
            for(i = 0; i < queueLen; i++) {
                if(me.taskEvents[i].length == 4
                   && me.taskEvents[i][0] == instanceNo
                   && me.taskEvents[i][3] == replaceId ) {

                    me.taskEvents[i][2] = eventData;
                    me.flushTaskEvents();
                    return me.taskEvents[1];
                }
            }
        }
        eventNo = me.nextSendEventNo++;
        if(replaceId) {
            me.taskEvents.push([instanceNo,eventNo,eventData,replaceId]);
        } else {
            me.taskEvents.push([instanceNo,eventNo,eventData]);
        }
        me.flushTaskEvents();
        return eventNo;
    },
    flushTaskEvents: function() {
        var me = this,
            params = {},
            xhr, instanceNo, event;
        if(!me.flushingTaskEvents && me.taskEvents.length) {
            event = me.taskEvents.shift();
            instanceNo = event[0];
            //Set event number
            params['eventNo'] = event[1];
            //Copy event params
            for(k in event[2]) {
                params[k] = event[2][k];
            }

            me.flushingTaskEvents = true;
            //Send request
            xhr = new XMLHttpRequest();
            xhr.open('POST',instanceNo + '/' + me.instances[instanceNo].instanceKey +'/gui'+me.urlParameters,true);
            xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
            xhr.onerror = me.onTaskEventError.bind(me);
            xhr.onload = me.onTaskEventResponse.bind(me);
            xhr.send(itwc.util.urlEncode(params));
        }
    },
    onTaskEventError: function(e) {
        itwc.controller.showError("Flushing events failed");
    },
    onTaskEventResponse: function(e) {
        var me = this,
            msg;
			
        msg = JSON.parse(e.target.responseText);
		
        if(msg.error) {
            console.log("Server error",msg.error);
        }
        //Update event no
        me.lastReceivedEventNo = msg.lastEvent;
        //Update user interface
        if(msg.updates) {
            me.controller.updateUI({instance: msg.instance,updates: msg.updates},me.instances[msg.instance].rootNode);
        }
        //Schedule automatic refresh when an expiration time is set
        //and we do not have a push event source
        if(!me.updateSource && !!window.EventSource) {
            me.startUIEventSource();
        } else {
            //TODO set a timer for a refresh
        }
        me.flushingTaskEvents = false;
        me.flushTaskEvents();
    },
    startUIEventSource: function() {
        var me = this;
        me.updateSource = new EventSource('gui-stream?instances='+Object.keys(me.instances).join(','));
        me.updateSource.onerror = me.onPushError.bind(me);
        me.updateSource.addEventListener('reset', me.onResetPushEvent.bind(me), false);
        me.updateSource.addEventListener('message', me.onUpdatePushEvent.bind(me), false);
    },
    restartUIEventSource: function() {
        var me = this;
        if(me.updateSource) {
            me.updateSource.close();
        }
        me.startUIEventSource();
    },
    stopUIEventSource: function() {
        var me = this;
        if(me.updateSource) {
            me.updateSource.close();
            me.updateSource = null;
        }
    },
    onPushError:function(e) {
        var me = this;
        me.stopUIEventSource();
        itwc.controller.showError("Disconnected from server");
    },
    onResetPushEvent: function(e) {
        var me = this;
        console.log("Task done!");
    },
    onUpdatePushEvent: function (e) {
        var me = this,
            msg = JSON.parse(e.data);			
        me.controller.updateUI(msg, me.instances[msg.instance].rootNode);
        me.flushingTaskEvents = false;
        me.flushTaskEvents();
    },
    sendResetEvent: function(instanceNo) {
        var me = this;
        return me.queueTaskEvent(instanceNo, {resetEvent: instanceNo});
    },
	sendEditEvent: function(taskId, editorId, value, replace) {
		var me = this,
            instanceNo = (taskId.split("-")[0] | 0);

		return me.queueTaskEvent(instanceNo,{editEvent: JSON.stringify([taskId,editorId,value])}, replace ? editorId : null );
	},
	sendActionEvent: function(taskId, actionId) {
		var me = this,
            instanceNo = (taskId.split("-")[0] | 0);
		return me.queueTaskEvent(instanceNo,{actionEvent: JSON.stringify([taskId,actionId])});
    },
    sendFocusEvent: function(taskId) {
        var me = this,
            instanceNo = (taskId.split("-")[0] | 0);
        return me.queueTaskEvent(instanceNo,{focusEvent: JSON.stringify(taskId)});
    }
});
itwc.taskletInstanceProxy = itwc.extend(itwc.taskInstanceProxy,{

    init: function(controller) {
        var me = this;
        me.controller = controller;
        me.nextSendEventNo = 0;
    },
    setRootNode: function(rootNode) {
        this.rootNode = rootNode;
    },
    sendResetEvent: function(instanceNo) {
        this.processEvent(instanceNo.toString(), "reset", this.nextSendEventNo++);
    },
	sendEditEvent: function(taskId, editorId, value) {
        this.processEvent(taskId, "edit", this.nextSendEventNo++, editorId, value);
    },
	sendActionEvent: function(taskId, actionId) {
        this.processEvent(taskId, "commit", this.nextSendEventNo++, actionId);
    },
    sendFocusEvent: function(taskId) {
        this.processEvent(taskId, "focus", this.nextSendEventNo++);
    },
    processEvent: function(taskId,eventType,eventNo,eventName,eventValue) {
	
	    console.time('controllerWrapper: eval');
        var me = this,
            instanceNo = taskId.split("-")[0],
    	    taskletId = instanceNo + "-0",
    	    state = me.rootNode.definition.st;
            controllerFunc = me.rootNode.definition.controllerFunc

	    var tmp = [controllerFunc,[]];
	    tmp[1].push(taskId);
	    tmp[1].push(state);
	
	    if(eventType == "focus" || eventType == "edit" || eventType == "commit") {
		    tmp[1].push([1, 'Just', eventNo])
	    } else {
		    tmp[1].push([0, 'Nothing']);
	    }
	    if(eventType == "edit" || eventType == "commit"){
		    tmp[1].push([1, 'Just', eventName])
	    }else{
		    tmp[1].push([0, 'Nothing']);
	    }
	    if(eventType == "edit"){
		    // convert eventValue to JSON to mediate type information to the controller (e.g. Int?)
		    tmp[1].push([1, 'Just', JSON.stringify(eventValue)])
	    } else {
		    tmp[1].push([0, 'Nothing']);
	    }	
	    tmp[1].push(_iworld);
	
    	// result is a tuple of mbUI and state
    	var ys = Sapl.feval(tmp);
	    _iworld = Sapl.feval(ys[4]);	
	    state = Sapl.heval(ys[3]);
	
	    me.rootNode.definition.st = state;	// save it
	
	    // toJS to make the result hyperstrict
        var newres = Sapl.toJS(Sapl.fapp(me.rootNode.definition.resultFunc,[state]));	
	
        var mbUI = Sapl.feval(ys[2]);
		
	    // If mbUI is Nothing, the task is finished
	    if(mbUI[0] == 0) {
		    itwc.controller.sendEditEvent(me.rootNode.definition.taskId, "finalize", newres);
	    } else {		
		    var upd = Sapl.feval(mbUI[2]);
		
            console.timeEnd('controllerWrapper: eval');
				
            console.time('controllerWrapper: apply UI update');
            itwc.controller.updateUI({instance: instanceNo, updates:JSON.parse(upd)}, me.rootNode);
            console.timeEnd('controllerWrapper: apply UI update');
		
            __itask_background_process();
		
            // Send result to the client if it is changed only
            if(!geq(me.rootNode.definition.lastResult, newres)){
    			me.rootNode.definition.lastResult = newres;
    			//itwc.controller.sendEditEvent(taskletId, "result", newres);
    		}		
	    }
    }
});
itwc.controller = function() {
    var me = this;

    // Client state
    // One of: disconnected/connected-eventsource/connected-poll
    me.state = 'disconnected';

    //Event queue and administration
    me.taskEvents = [];
    me.nextSendEventNo = 0;
    me.flushingTaskEvents = false;
    me.refresher = null;
    me.updateSource = null;
    me.urlParameters = '';

    //Global set of layers
    me.layers = [];
    me.errorLayer = [];

    //The shared remote instance proxy
    me.remoteProxy = null;
};
itwc.controller.prototype = {

    //Global set of layers
    layers: [],

	//Global registration of tasklets & editlets
    editlets: {},
	tasklets: {},

    //The set of task instance proxies which handle events & UI updates
    instanceProxies: {},

    sendEditEvent: function(taskId, editorId, value, replace) {
        var me = this,
            instanceNo = taskId.split("-")[0];
		
        if(me.instanceProxies[instanceNo]) {
            return me.instanceProxies[instanceNo].sendEditEvent(taskId,editorId,value,replace);
        }
    },
    sendActionEvent: function(taskId, actionId) {
        var me = this,
            instanceNo = taskId.split("-")[0];

        if(me.instanceProxies[instanceNo]) {
            return me.instanceProxies[instanceNo].sendActionEvent(taskId,actionId);
        }
    },
    sendFocusEvent: function(taskId) {
        var me = this,
	        instanceNo = taskId.split("-")[0];	

        if(me.instanceProxies[instanceNo]) {
            return me.instanceProxies[instanceNo].sendFocusEvent(taskId);
        }
    },	
    updateUI: function(update,root) {
        var me = this,
            instance = update.instance,
            updates = update.updates,
            cmp;

        updates.forEach(function(update) {
            cmp = me.findComponent(update.path,root);

            if(!cmp) {
                console.log("Could not find component at path",update.path);
                return;
            }
            //Apply operations
            //'add' and 'remove' are handled directly by the controller
            //other operations are passed to the component to deal handle
            update.operations.forEach(function(op) {
                switch(op.method) {
                    case 'add':
                        me.addComponent(cmp,op.arguments[0],op.arguments[1], op.arguments[0] == cmp.items.length);
                        //Call initializion function that needs to be applied after the full component and
                        //its children are available in the DOM
                        cmp.items[op.arguments[0]].afterAdd();
                        break;
                    case 'remove':
                        me.removeComponent(cmp,op.arguments[0], op.arguments[0] == cmp.items.length - 1);
                        break;
                    case 'replace':
                        me.removeComponent(cmp,op.arguments[0], op.arguments[0] == cmp.items.length - 1);
                        me.addComponent(cmp,op.arguments[0],op.arguments[1], op.arguments[0] == cmp.items.length);
                        cmp.items[op.arguments[0]].afterAdd();
                        break;
                    case 'addWindow':
                        me.addWindow(cmp,op.arguments[0],op.arguments[1]);
                        cmp.windows[op.arguments[0]].afterAdd();
                        break;
                    case 'removeWindow':
                        me.removeWindow(cmp,op.arguments[0]);
                        break;
                    case 'addMenu':
                        me.addMenu(cmp,op.arguments[0]);
                        break;
                    case 'removeMenu':
                        me.removeMenu(cmp,op.arguments[0]);
                        break;
                    default:
                        cmp.applyUpdate(op.method,op.arguments);
                        break;
                }
            });
        });
    },
    //Apply update instructions to global ui tree.
    findComponent: function(path,root) {
        var cmp;
        if(path.length && path[0] === 'w') {
            cmp = root.windows[path[1]];
            path.splice(0,2);
            if(!cmp) {
                console.log("Window not found");
                return cmp;
            }
        } else {
            cmp = root ? root : itwc.UI;
        }
        path.forEach(function(step) {
            cmp = (step === 'm') ? cmp.menu : cmp.items[step];
        });
        return cmp;
    },
    addComponent: function(parentCmp,insertIdx,insertDef,isLast) {
        var me = this, newCmp, prevIdx;

        //Create component
        if(itwc.component[insertDef.xtype]) {
            newCmp = new itwc.component[insertDef.xtype]();
        } else {
            console.log("Unknown component type ",insertDef.xtype,insertDef);
            newCmp = new itwc.Component();
        }
        newCmp.init(insertDef,parentCmp);

        //Add component to parent
        parentCmp.items.splice(insertIdx,0,newCmp);

        //Render the DOM element
        newCmp.render(insertIdx,isLast);

        //Correct sibling margins
        if(insertIdx > 0) {
            prevIdx = insertIdx - 1;
            parentCmp.items[prevIdx].initMargins(prevIdx,false);
        }
        /*
        if(!isLast) {
            nextIdx = insertIdx + 1;
            parentCmp.items[prevIdx].restore
        }
        */

        //Recursively add children
        if(insertDef.items) {
            insertDef.items.forEach(function(childCmp,childIdx) {
                me.addComponent(newCmp,childIdx,childCmp);
            });
        }
        //Recursively add menu items
        if(newCmp.menu && insertDef.menu) {
            insertDef.menu.forEach(function(childCmp,childIdx) {
                me.addComponent(newCmp.menu,childIdx,childCmp);
            });
        }
        if(parentCmp.afterItemAdded) {
            parentCmp.afterItemAdded(insertIdx,newCmp);
        }
        //Inject the component's DOM element in its parent component's
        if(isLast) {
            parentCmp.targetEl.appendChild(newCmp.domEl);
        } else {
            parentCmp.targetEl.insertBefore(newCmp.domEl,parentCmp.targetEl.childNodes[insertIdx]);
        }
    },
    removeComponent: function(parentCmp,removeIdx,isLast) {
        var prevIdx;

        //Call pre-removal function
        parentCmp.items[removeIdx].beforeDOMRemove();

        //We need to take the number of non-child component nodes (like a header) of the parent into account
        parentCmp.targetEl.removeChild(parentCmp.targetEl.childNodes[removeIdx]);

        //Remove from component tree
        parentCmp.items.splice(removeIdx,1);

        if(parentCmp.afterItemRemoved) {
            parentCmp.afterItemRemoved(removeIdx);
        }
    },
    addWindow: function(cmp,winIdx,winDef) {
        var me = this,
            win,top,left;

        win = new itwc.component.itwc_window();
        win.init(winDef,cmp);
        win.render(winIdx,false);

        //Add window's children
        if(winDef.items) {
            winDef.items.forEach(function(childCmp,childIdx) {
                me.addComponent(win,childIdx,childCmp);
            });
        }
        //Add window' menus
        if(win.menu && winDef.menu) {
            winDef.menu.forEach(function(childCmp,childIdx) {
                me.addComponent(win.menu,childIdx,childCmp);
            });
        }
        //Register references
        cmp.windows[winIdx] = win;
        me.layers.push(win);

        //Inject in DOM
        document.body.appendChild(win.domEl);

        //Position window
        switch(winDef.vpos || win.defaultVpos) {
            case 'top': top = win.margins[0]; break;
            case 'middle': top = (document.body.offsetHeight / 2) - (win.panelEl.offsetHeight / 2); break;
            case 'bottom': top = document.body.offsetHeight - win.panelEl.offsetHeight - win.margins[2]; break;
        }
        switch(winDef.hpos || win.defaultHpos) {
            case 'left': left = win.margins[3]; break;
            case 'center': left = ((document.body.offsetWidth / 2) - (win.panelEl.offsetWidth / 2)); break;
            case 'right': left = document.body.offsetWidth - win.panelEl.offsetWidth - win.margins[1]; break;
        }
        win.panelEl.style.top = top + 'px';
        win.panelEl.style.left = left + 'px';

        return win;
    },
    removeWindow: function(cmp,winIdx) {
        document.body.removeChild(cmp.windows[winIdx].domEl);
        cmp.windows.splice(winIdx,1);
    },
    addMenu: function(cmp,items) {
        var me = this;
        //Create menu
        cmp.setMenu(items);
        //Add menu items
        if(cmp.menu) {
            items.forEach(function(childCmp,childIdx) {
                me.addComponent(cmp.menu,childIdx,childCmp);
            });
        }
    },
    removeMenu: function(cmp) {
        cmp.setMenu(null);
    },
    showError: function(error) {
        var me = this;

        if(me.errorWindow) {
            me.errorWindow.items[0].setValue(error);
        } else {
            me.errorWindow = me.addWindow(me.layers[0],0,{
                xtype: 'itwc_window',
                windowType: 'modal',
                title: 'Connection error',
                itwcWidth: 300,
                items: [{xtype:'itwc_view_html',value: error, margins: '5 5 5 5'}
                       ,{xtype:'itwc_container',halign: 'right',itwcWidth: 'flex',padding: '5 5 5 5'
                        ,items:[{xtype:'itwc_clientbutton',text:'Reset',actionId:'reset'}]
                        }
                       ]
            });
        }
    },
    clearError: function() {
        var me = this;
        if(me.errorLayer) {
            document.body.removeChild(me.errorLayer);
            me.errorLayer = null;
        }
    },
    reset: function() {
        window.location.reload();
    },
    onWindowResize: function(e) {
        var me = this,
            width = window.innerWidth,
            height = window.innerHeight, num, i, layer;
        num = me.layers.length;
        for(i = 0; i < num; i++) {
            layer = me.layers[i];
            if(layer.maximize || layer.modal) {
                layer.domEl.style.width = width;
                layer.domEl.style.height = height;
                layer.afterResize();
            }
        }
    },
    start: function () {
        var me = this;

        //Initialize the client component tree and DOM
        itwc.DOMID = 1000;

        //Empty instance proxy list
        me.instanceProxies = [];

        //Empty layer list
        me.layers = [];

        //Create the shared remote proxy
        me.remoteProxy = new itwc.remoteInstanceProxy();
        me.remoteProxy.init(me);

        //Create the viewport layer
        me.layers[0] = new itwc.component.itwc_viewport();
        me.layers[0].init({instanceNo: itwc.START_INSTANCE_NO, instanceKey: itwc.START_INSTANCE_KEY, halign: 'center', valign: 'middle'});
        me.layers[0].render(0,false);

        document.body.appendChild(me.layers[0].domEl);

        //Send empty event to synchronize the start instance
        me.remoteProxy.sendResetEvent(itwc.START_INSTANCE_NO);

        //Listen for changes in the viewport size
        window.addEventListener('resize',me.onWindowResize.bind(me));
        //Listen for global errors
        window.onerror = function(msg,url,line) {
        }
    }
};
//Set up a singleton controller object
itwc.controller = new itwc.controller();
itwc.global.controller = itwc.controller; //Backwards compatibility

//Start the controller when the bootstrap page has loaded
window.onload = function() {
    itwc.controller.start();
};
