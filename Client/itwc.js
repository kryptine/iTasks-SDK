//#### iTasks Web Client ####//
//This javascript program defines the web-based run-time environment of iTasks programs
itwc = {};

//#### UTILITY FUNCTIONS ####//
itwc.util = {};
itwc.util.urlEncode = function (obj) {
    var parts = [];
    for(k in obj) {
        parts.push(k+'='+encodeURIComponent(obj[k]));
    }
    return parts.join('&');
}
//Define a new prototype object, inheriting the prototype of an existing one
itwc.util.define = function(inheritFrom,definition) {
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

    init: function(definition, parentCmp ) {
        var me = this;

        me.type = (definition && definition.xtype) || 'itwc_component';
        me.domEl = null;
        me.targetEl = null;
        me.parentCmp = parentCmp || null;
        me.items = [];
        me.definition = definition || {};
    },
    render: function(itemIdx) {
        var me = this;
        me.domId = 'itwc-' + itwc.DOMID++;

        //Create and initialize dom element
        me.domEl = document.createElement(me.domTag);
        me.targetEl = me.domEl;

        me.initDOMEl(itemIdx);
        me.initSize();
        me.domEl.id = me.domId;
    },
    //Configurable properties for initializing the DOM element
    domTag: 'div',
    initDOMEl: function(itemIdx) {
    },
    initSize: function() {
        var me = this,
            el = me.domEl,
            width = me.definition.itwcWidth || me.defaultWidth,
            height = me.definition.itwcHeight || me.defaultHeight,
            direction = me.parentCmp.definition.direction || me.parentCmp.defaultDirection;
        //Set width
        if(width === 'flex') {
            if(direction == 'horizontal') {
                el.style.flex = 1;
            } else {
                el.style.alignSelf = 'stretch';
            }
        } else if (width === 'wrap') {
        } else {
            el.style.width = width + 'px';
        }
        //Set height
        if(height === 'flex') {
            if(direction == 'vertical') {
                el.style.flex = 1;
            } else {
                el.style.alignSelf = 'stretch';
            }
        } else if (height === 'wrap') {
        } else {
            el.style.height = height + 'px';
        }
        //Set margins
        if(me.definition.margins) {
            el.style.margin = me.definition.margins.split(' ').map(function(s) { return s + 'px'}).join(' ');
        }
        if(me.definition.padding) {
            el.style.padding = me.definition.padding.split(' ').map(function(s) { return s + 'px'}).join(' ');
        }
        //If a container, set alignment
        if(me.isContainer) {
            me.initItemLayout();
        }
    },
    initItemLayout: function() {
        var me = this,
            el = me.domEl;

        el.classList.add(me.definition.direction == 'horizontal' ? 'hcontainer' : 'vcontainer');
        //Horizontal alignment
        if(me.definition.halign) {
            if(me.definition.direction == 'horizontal') {
                switch(me.definition.halign) {
                    case 'left':    el.style.justifyContent = 'flex-start'; break;
                    case 'center':  el.style.justifyContent = 'center'; break;
                    case 'right':   el.style.justifyContent = 'flex-end'; break;
                }
            } else {
                //TODO
            }
        }
        //Vertical alignment
        //TODO
    },
    afterAdd: function() {
        var me = this;
        if(me.items && me.items.length) {
            me.items.forEach(function(cmp) {
               cmp.afterAdd();
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
    applyUpdate: function(operation,args) {
        var me = this;
        if(me[operation] && typeof me[operation] == 'function') {
            me[operation].apply(me,args);
        } else {
            console.log("Unsupported operation on component",operation,args);
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
    }
}
itwc.Container = itwc.util.define(itwc.Component,{
    isContainer: true,
    defaultDirection: 'vertical',
    itemsOffset: 0,                  //The number of DOM elements before the child items are positioned (for example 1 if a panel has a header)
    afterItemAdded: null,
    afterItemRemoved: null
});
itwc.ExtComponent = itwc.util.define(itwc.Component,{
    afterAdd: function() {
        var me = this,
            cfg = {}, k, divEl;

        for(k in me.definition) {
            cfg[k] = me.definition[k];
        }
        cfg.itwcCmp = me;
        cfg.renderTo = me.domEl;

        divEl = Ext.get(me.domEl);

        me.extCmp = Ext.create(me.extClass,cfg);
        me.extCmp.setSize(divEl.getSize());
        me.extCmp.show();
    },
    afterResize: function() {
        var me = this,
            divEl;

        if(me.extCmp) {
            divEl = Ext.get(me.domEl);
            me.extCmp.setSize(divEl.getSize());
        }
    },
    setValue: function(value) {
        this.extCmp.setValue(value);
    },
    setTaskId: function(taskId) {
        this.extCmp.taskId = taskId;
    }
});
//#### CORE UI COMPONENT DEFINITIONS ####//
itwc.component = {};
itwc.component.itwc_viewport = itwc.util.define(itwc.Container,{
    render: function() {
        var me = this, i;
        me.domEl = document.body;
        me.targetEl = document.body;
        //Make sure the body is really empty
        for(i = me.domEl.childNodes.length - 1; i >= 0; i--) {
            me.domEl.removeChild(me.domEl.childNodes[i]);
        }
    }
});
itwc.component.itwc_view_string = itwc.util.define(itwc.Component,{
    defaultWidth: 'wrap',
    initDOMEl: function() {
        this.domEl.innerHTML = this.definition.value;
    },
    setValue: function(value) {
        this.domEl.innerHTML = value;
    }
});
itwc.component.itwc_view_html = itwc.util.define(itwc.Component,{
    initDOMEl: function() {
        this.domEl.innerHTML = this.definition.value;
    },
    setValue: function(value) {
        this.domEl.innerHTML = value;
    }
});
itwc.component.itwc_edit_string = itwc.util.define(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.addEventListener('keyup',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
    }
});
itwc.component.itwc_edit_password = itwc.util.define(itwc.Component,{
    domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'password';
        el.addEventListener('keyup',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
    }
});
itwc.ButtonComponent = itwc.util.define(itwc.Component,{
    domTag: 'a',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,icon,label;

        el.classList.add('button');
        el.href = '#';

        if(me.definition.iconCls) {
            icon = document.createElement('div');
            icon.classList.add('button-icon');
            icon.classList.add(me.definition.iconCls);
            el.appendChild(icon);
        }
        me.disabled = me.definition.disabled || false;
        if(me.disabled) {
            el.classList.add('button-disabled');
        }
        if(me.definition.text) {
            label = document.createElement('div');
            label.innerHTML = me.definition.text;
            label.classList.add('button-label');
            el.appendChild(label);
        }
        el.addEventListener('click',function(e) {
            if(!me.disabled) {
                me.onClick(e);
            }
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
    }
});

itwc.component.itwc_actionbutton = itwc.util.define(itwc.ButtonComponent,{
    onClick: function (e) {
        var me = this;
        itwc.controller.sendActionEvent(me.definition.taskId,me.definition.actionId);
    }
});
itwc.component.itwc_editbutton = itwc.util.define(itwc.ButtonComponent,{
    onClick: function (e) {
        var me = this;
        itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,me.definition.value);
    }
});
itwc.component.itwc_icon= itwc.util.define(itwc.Component,{
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.classList.add('icon');
        el.classList.add(me.definition.iconCls);
        me.currentIcon = me.definition.iconCls;

        if(me.definition.tooltip) {
            el.setAttribute('data-hint',me.definition.tooltip);
            el.classList.add('hint--left');
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
        el.setAttribute('data-hint',tooltip);
        el.classList.add('hint--left');
    }
});
itwc.component.itwc_container = itwc.util.define(itwc.Container,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        if(me.definition.baseCls) {
            el.classList.add(me.definition.baseCls);
        }
    }
});
itwc.component.itwc_panel = itwc.util.define(itwc.Container,{
    defaultHeight: 'flex',
    initDOMEl: function() {
        var me = this,
            el = me.domEl, header;

        if(me.definition.baseCls) {
            el.classList.add(me.definition.baseCls);
        }
        el.classList.add('panel');
        if(me.definition.frame) {
            el.classList.add('framed');
        }
        if(me.definition.title) {
            header = document.createElement('div');
            header.innerHTML = me.definition.title;
            header.classList.add('panel-header');

            el.appendChild(header);
            me.itemsOffset = 1;
        }
    },
    setTitle: function(title) {
        //TODO: Handle case in which we don't have a title div yet
        this.domEl.childNodes[0].innerHTML = title;
    }
});
itwc.component.itwc_tabset = itwc.util.define(itwc.Container,{
    isContainer: false, //Don't size as container
    itemsOffset: 1,
    defaultWidth: 'flex',
    defaultHeight: 'flex',
    activeTab: 0,

    initDOMEl: function() {
        var me = this,
            el = me.domEl, bar;
        el.classList.add('tabset');

        bar = document.createElement('ul');
        bar.classList.add('tabbar');

        el.appendChild(bar);
        me.tabBar = bar;
    },
    afterItemAdded: function(itemIdx,itemCmp) {
        var me = this,
            tab,label,closeLink;

        //Add a tab
        tab = document.createElement('li');

        label = document.createElement('a');
        label.innerHTML = itemCmp.definition.title;
        label.href = '#';
        if(itemCmp.definition.focusTaskId) {
            label.addEventListener('click',function(e) {
                itwc.controller.sendFocusEvent(itemCmp.definition.focusTaskId);
                return false;
            },me);
        }
        tab.appendChild(label);

        if(itemCmp.definition.closeTaskId) {
            closeLink = document.createElement('a');
            closeLink.innerHTML = 'x';
            closeLink.href = '#';
            closeLink.classList.add('tabclose');
            closeLink.addEventListener('click',function(e) {
                itwc.controller.sendActionEvent(itemCmp.definition.closeTaskId,'Close');
                return false;
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
        }
    }
});
itwc.component.itwc_tabitem = itwc.util.define(itwc.Container,{

    initDOMEl: function(itemIdx) {
        var me = this,
            el = me.domEl;

        el.classList.add('tabitem');
        if(itemIdx === me.parentCmp.activeTab) {
            el.classList.add('selected');
        }
    },
    initSize: function() {
        //Size is managed with css by tabset container
        //but we need to set alignment and direction for the items
        //in the tab
        this.initItemLayout();
    },
    setTitle: function(title) {
        var me = this;
        me.definition.title = title;

        //Update label of tab in DOM
        me.parentCmp.tabBar.childNodes[me.getChildIndex()].childNodes[0].innerHTML = title;
    },
    setFocusTaskId: function(taskId) {
        var me = this;
        me.definition.focusTaskId = taskId;
    },
    setCloseTaskId: function(closeTaskId) {
        var me = this;
        me.definition.closeTaskId = closeTaskId;
    }
});
itwc.component.itwc_choice_tree = itwc.util.define(itwc.ExtComponent,{
    extClass: 'itwc.component.choice.Tree'
});
itwc.component.itwc_choice_grid = itwc.util.define(itwc.ExtComponent,{
    extClass: 'itwc.component.choice.Grid'
});
//#### CENTRAL CONTROLLER ####//
itwc.controller = function() {
    var me = this;

    //Event queue and administration
    me.taskEvents = [];
    me.nextSendEventNo = 0;
    me.flushingTaskEvents = false;
    me.refresher = null;
    me.updateSource = null;

    //Tasklets & editlets
    me.taskletControllers = {};
};
itwc.controller.prototype = {

    initUI: function () {
        //Initialize the client component tree and DOM
        itwc.DOMID = 1000;
        itwc.TASKLETS = {};
        itwc.EDITLETS = {};

        itwc.UI = new itwc.component.itwc_viewport();

        itwc.UI.init();
        itwc.UI.render(0);

        //Listen for changes in the viewport size
        window.addEventListener('resize',this.onWindowResize,this);
    },
    onWindowResize: function(e) {
        itwc.UI.afterResize();
    },
    //Queue a task event for processing
    queueTaskEvent: function (eventData) {
        var me = this,
            eventNo;

        eventNo = me.nextSendEventNo++;
        me.taskEvents.push([eventNo,eventData]);
        me.flushTaskEvents();
    },
    sendEditEvent: function(taskId, editorId, value) {
        var me = this;
        me.queueTaskEvent({editEvent: JSON.stringify([taskId,editorId,value])});
    },
    sendActionEvent: function(taskId, actionId) {
        var me = this;
        me.queueTaskEvent({actionEvent: JSON.stringify([taskId,actionId])});
    },
    sendFocusEvent: function(taskId) {
        var me = this;
        me.queueTaskEvent({focusEvent: JSON.stringify(taskId)});
    },
    flushTaskEvents: function() {
        var me = this,
            params = {},
            xhr, event;
        if(!me.flushingTaskEvents & me.taskEvents.length) {
            event = me.taskEvents.shift();
            //Set event number
            params['eventNo'] = event[0];
            //Copy event params
            for(k in event[1]) {
                params[k] = event[1][k];
            }
            if(me.session) {
                params['session'] = me.session;
            }
            me.flushingTaskEvents = true;
            //Send request
            xhr = new XMLHttpRequest();
            xhr.open('POST','/?format=json-gui', true);
            xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
            xhr.onload = me.receiveTaskUpdates.bind(me);
            xhr.send(itwc.util.urlEncode(params));
        }
    },
    receiveTaskUpdates: function(e) {
        var me = this,
            msg;

        msg = JSON.parse(e.target.responseText);

        if(msg.error) {
            console.log("Server error",msg.error);
            return;
        }
        //Update session id
        me.session = msg.session;
        //Update event no
        me.lastReceivedEventNo = msg.lastEvent;
        //Update user interface
        me.updateUI(msg.updates);
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
        me.updateSource = new EventSource('/?format=json-gui-events&session='+me.session);
        me.updateSource.addEventListener('reset', me.onResetPushEvent.bind(me), false);
        me.updateSource.addEventListener('message', me.onUpdatePushEvent.bind(me), false);
    },
    onResetPushEvent: function(e) {
    },
    onUpdatePushEvent: function (e) {
        var me = this;
        me.updateUI(JSON.parse(e.data));
    },
    updateUI: function(updates) {
        var me = this,
            cmp;

        updates.forEach(function(update) {
            cmp = me.findComponent(update.path);

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
                        me.addComponent(cmp,op.arguments[0],op.arguments[1]);
                        //Call initializion function that needs to be applied after the full component and
                        //its children are available in the DOM
                        cmp.afterAdd();
                        break;
                    case 'remove':
                        me.removeComponent(cmp,op.arguments[0]);
                        break;
                    case 'replace':
                        me.removeComponent(cmp,op.arguments[0]);
                        me.addComponent(cmp,op.arguments[0],op.arguments[1]);
                        cmp.afterAdd();
                        break;
                    default:
                        cmp.applyUpdate(op.method,op.arguments);
                        break;
                }
            });
        });
    },
    //Apply update instructions to global ui tree.
    findComponent: function(path) {
        var cmp = itwc.UI;
        path.forEach(function(step) {
            cmp = cmp.items[step];
        });
        return cmp;
    },
    addComponent: function(parentCmp,insertIdx,insertDef) {
        var me = this,
            newCmp, domIdx;

        //Create component
        if(itwc.component[insertDef.xtype]) {
            newCmp = new itwc.component[insertDef.xtype]();
        } else {
            newCmp = new itwc.Component();
        }
        newCmp.init(insertDef,parentCmp);

        //Add component to parent
        parentCmp.items.splice(insertIdx,0,newCmp);

        //Render the DOM element
        newCmp.render(insertIdx);

        //Recursively add children
        if(insertDef.items) {
            insertDef.items.forEach(function(childCmp,childIdx) {
                me.addComponent(newCmp,childIdx,childCmp);
            });
        }
        if(parentCmp.afterItemAdded) {
            parentCmp.afterItemAdded(insertIdx,newCmp);
        }

        //Inject the component's DOM element in its parent component's
        domIdx = insertIdx + (parentCmp.itemsOffset || 0);
        if(parentCmp.targetEl.childNodes.length === domIdx) {
            parentCmp.targetEl.appendChild(newCmp.domEl);
        } else {
            parentCmp.targetEl.insertBefore(newCmp.domEl,parentCmp.targetEl.childNodes[domIdx]);
        }
    },
    removeComponent: function(parentCmp,removeIdx) {
        //Remove from DOM
        //We need to take the number of non-child component nodes (like a header) of the parent into account
        parentCmp.targetEl.removeChild(parentCmp.targetEl.childNodes[removeIdx + (parentCmp.itemsOffset || 0)]);
        //Remove from component tree
        parentCmp.items.splice(removeIdx,1);

        if(parentCmp.afterItemRemoved) {
            parentCmp.afterItemRemoved(removeIdx);
        }
    },
    start: function () {
        var me = this;
        //Initialize user interface data structures and DOM
        me.initUI();
        //Start a session by sending a blank event
        me.queueTaskEvent({});
    }
};

//Set up a singleton controller object
itwc.controller = new itwc.controller();

//Start the controller when the bootstrap page has loaded
window.onload = function() {
    itwc.controller.start();
};

