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
}
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

    init: function(definition, parentCmp ) {
        var me = this;

        me.type = (definition && definition.xtype) || 'itwc_component';
        me.domEl = null;
        me.targetEl = null;
        me.parentCmp = parentCmp || null;
        me.items = [];
        me.definition = definition || {};
        me.hotkeyListener = null;
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
                el.style.webkitFlex = 1;
            } else {
                el.style.alignSelf = 'stretch';
                el.style.webkitAlignSelf = 'stretch';
            }
        } else if (width === 'wrap') {
        } else {
            el.style.width = width + 'px';
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
            el = me.targetEl;

        el.classList.add(me.definition.direction == 'horizontal' ? 'hcontainer' : 'vcontainer');
        //Horizontal alignment
        if(me.definition.halign) {
            if(me.definition.direction == 'horizontal') {
                switch(me.definition.halign) {
                    case 'left':    el.style.justifyContent = 'flex-start'; el.style.webkitJustifyContent = 'flex-start'; break;
                    case 'center':  el.style.justifyContent = 'center'; el.style.webkitJustifyContent = 'center'; break;
                    case 'right':   el.style.justifyContent = 'flex-end'; el.style.webkitJustifyContent = 'flex-end'; break;
                }
            } else {
                //TODO
            }
        }
        //Vertical alignment
        if(me.definition.valign) {
            if(me.definition.direction == 'vertical') {
                switch(me.definition.valign)  {
                    case 'top':     el.style.justifyContent = 'flex-start'; el.style.webkitJustifyContent = 'flex-start'; break;
                    case 'middle':  el.style.justifyContent = 'center'; el.style.webkitJustifyContent = 'center'; break;
                    case 'bottom':   el.style.justifyContent = 'flex-end'; el.style.webkitJustifyContent = 'flex-end'; break;
                }
            } else {
                //TODO
            }
        }
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
    }

}
itwc.Container = itwc.extend(itwc.Component,{
    isContainer: true,
    defaultDirection: 'vertical',
    itemsOffset: 0,                  //The number of DOM elements before the child items are positioned (for example 1 if a panel has a header)
    afterItemAdded: null,
    afterItemRemoved: null
});
//#### CORE UI COMPONENT DEFINITIONS ####//
itwc.component = {};

itwc.component.itwc_menubar = itwc.extend(itwc.Container, {
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
        me.menu.init(me.definition.menu,me);
        me.menu.render(0);
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
itwc.component.itwc_viewport = itwc.extend(itwc.Container,{
    render: function() {
        var me = this, i;
        me.domEl = document.body;
        me.targetEl = document.body;
        //Make sure the body is really empty
        for(i = me.domEl.childNodes.length - 1; i >= 0; i--) {
            me.domEl.removeChild(me.domEl.childNodes[i]);
        }
    },
    setTitle: function(title) {
        document.title = title;
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
        el.value = me.definition.cur;
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
            me.setValue(value);
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
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
    },
    setEditorValue: function(value) {
		var receivedNo = itwc.controller.lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
            this.domEl.value = value;
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
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
    },
    setEditorValue: function(value) {
        var receivedNo = itwc.controller.lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
            this.domEl.value = value;
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
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
    },
    setEditorValue: function(value) {
        var receivedNo = itwc.controller.lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;
		if(receivedNo > sentNo) {
            this.domEl.value = value;
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
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.checked);
        });
    },
    setValue: function(value) {
        this.domEl.checked = value;
    },
    setEditorValue: function(value) {
        this.domEl.checked = value;
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
                value = me.allowDecimal ? parseFloat(e.target.value) : parseInt(e.target.value);
            }
            me.lastEditNo = itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,value);
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
        var receivedNo = itwc.controller.lastReceivedEventNo,	
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
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
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
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,e.target.value === "" ? null : e.target.value);
        });
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
        el.value = me.definition.cur;

        el.addEventListener('change',function(e) {
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,parseInt(e.target.value));
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
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,null);
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
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,rsp[0]);
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

        me.htmlId = "editlet-" + me.definition.taskId + "-" + me.definition.editorId;
		itwc.controller.editlets[me.htmlId] = me;

        el.innerHTML = me.definition.html;

        //Prepare javascript
        if(me.definition.script != null && me.definition.script != "" && !sapldebug) {
            evalScript(me.definition.script);
			delete me.definition.script;
			_dynamic_hijack();					
        }
        if(me.definition.defVal != null) {
            eval("tmp = " + me.definition.defVal + ";");
            me.value = Sapl.feval([tmp,[0]]); // the actual argument doesnt matter
            delete me.definition.defVal;
        }
        if(me.definition.appDiff != null) {
            eval("tmp = " + me.definition.appDiff + ";");
            me.appDiff = tmp;
            delete me.definition.appDiff;
        }
        if(me.definition.genDiff != null) {
            eval("tmp = " + me.definition.genDiff + ";");
            me.genDiff = tmp;
            delete me.definition.genDiff;
        }
        if(me.definition.initDiff != null) {
            eval("tmp = " + me.definition.initDiff + ";");
            me.initDiff = tmp;
            delete me.definition.initDiff;
        }
        if(me.definition.updateUI != null) {
            eval("tmp = " + me.definition.updateUI + ";");
            me.updateUI = tmp;
            delete me.definition.updateUI;
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

        if(me.initDiff != null) {
			if(me.initDiff[0]==1) {
				me.value = Sapl.feval([me.appDiff,[me.initDiff[2],me.value]]);
            }	
		    me.fireUpdateEvent(me.initDiff);
		} else {
			me.fireUpdateEvent(__Data_Maybe_Nothing);
		}

        //TEMPORARY FOR EXTJS STUFF
        if(me.eventAfterLayout) {
            me.eventAfterLayout.apply(me,["dummy event"]);
        }
    },
    afterShow: function() {},
    fireUpdateEvent: function (mbDiff) {
		var me = this;
		(me.eventHandler(false,me.updateUI))(mbDiff);		
	},
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
			Sapl.feval(ys[2]);
			Sapl.feval(ys[3]);
			//Determine diff before overwriting me.value (using superstrict evaluation)
			var diff = me.jsFromSaplJSONNode(Sapl.heval([me.genDiff,[me.value,ys[2]]]));

			me.value = ys[2];
			//Synchronize
			if(diff !== null) {
				itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,diff);
			}
		};
		return h;
	},
    applyDiff: function(saplDiff,extraJS) {

        var me = this,
            tmp;
        if(extraJS != "") {
            evalScript(extraJS);
        }
        eval("tmp = " + saplDiff + ";");

		if(tmp[0]==1) {
		    me.value = Sapl.feval([me.appDiff,[tmp[2],me.value]]);
        }	
        me.fireUpdateEvent(tmp);
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
            el = me.domEl, tmp;

		if(me.definition.html) {
			el.innerHTML = me.definition.html;
        } else if(me.definition.tbar) {
            el.classList.add('vcontainer');
            me.menu = new itwc.component.itwc_menubar();
            me.menu.init({xtype: "itwc_menu_bar", items: me.definition.tbar}, me);
            me.menu.render(0);
            el.appendChild(me.menu.domEl);

            me.targetEl = document.createElement('div');
            me.targetEl.style.flex = 1;
            el.appendChild(me.targetEl);
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
				_iworld = Sapl.fapp(__iTasks_Framework_Client_RunOnClient_createClientIWorld, [me.definition.instanceNo]);
			}				
		
			console.time('controllerWrapper timer: eval');
				
			eval("tmp = " + me.definition.controllerFunc + ";");
			me.definition.controllerFunc = tmp;
			itwc.controller.taskletControllers[me.definition.instanceNo] = me;
						
			var ret = Sapl.fapp(me.definition.controllerFunc, [me.definition.taskId, me.definition.st, __Data_Maybe_Nothing, __Data_Maybe_Nothing, __Data_Maybe_Nothing, _iworld]);
			me.definition.st = Sapl.feval(ret[3]);
			_iworld = Sapl.feval(ret[4]);
			
			var ui = Sapl.toJS(Sapl.feval(ret[2]));
			
			console.timeEnd('controllerWrapper timer: eval');
			
			console.time('controllerWrapper timer: apply UI');
			itwc.controller.updateUI(JSON.parse(ui), me);
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

        if(iconCls === null) {
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
        itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,me.definition.value);
    },
    setEditorValue: function(value) {
        var me = this;
        me.definition.value = value;
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
itwc.component.itwc_panel = itwc.extend(itwc.Container,{
    defaultHeight: 'flex',
    hasTitle: false,
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
            me.createTitle(me.definition.title);
        }
        if(me.definition.tbar) {
            el.classList.add('vcontainer');
            me.menu = new itwc.component.itwc_menubar();
            me.menu.init({xtype: "itwc_menu_bar", items: me.definition.tbar}, me);
            me.menu.render(0);
            el.appendChild(me.menu.domEl);

            me.targetEl = document.createElement('div');
            me.targetEl.style.flex = 1;
            el.appendChild(me.targetEl);
        }
    },
    createTitle: function(title) {
        var me = this, header;

        header = document.createElement('div');
        header.innerHTML = title;
        header.classList.add('panel-header');

        if(me.targetEl.childNodes.length) {
            me.targetEl.insertBefore(header,me.targetEl.childNodes[0]);
        } else {
            me.targetEl.appendChild(header);
        }
        me.hasTitle = true;
        me.itemsOffset = 1;
    },
    setTitle: function(title) {
        var me = this;
        if(me.hasTitle) {
            me.domEl.childNodes[0].innerHTML = title;
        } else {
            me.createTitle(title);
        }
    }
});
itwc.component.itwc_tabset = itwc.extend(itwc.Container,{
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
            tab,icon,label,closeLink;

        //Add a tab
        tab = document.createElement('li');

        label = document.createElement('a');
        label.innerHTML = itemCmp.definition.title;
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
itwc.component.itwc_tabitem = itwc.extend(itwc.Container,{

    initDOMEl: function(itemIdx) {
        var me = this,
            el = me.domEl;

        el.classList.add('tabitem');
        if(itemIdx === me.parentCmp.activeTab) {
            el.classList.add('selected');
        }
        if(me.definition.tbar) {
            el.classList.add('vcontainer');
            me.menu = new itwc.component.itwc_menubar();
            me.menu.init({xtype: "itwc_menu_bar", items: me.definition.tbar}, me);
            me.menu.render(0);
            el.appendChild(me.menu.domEl);

            me.targetEl = document.createElement('div');
            me.targetEl.style.flex = 1;
            el.appendChild(me.targetEl);
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
itwc.component.itwc_window = itwc.extend(itwc.Container,{
    initDOMEl: function() {
        var me = this,
            el = me.domEl, header, label, closeLink;

        el.classList.add('window');

        if(me.definition.title || me.definition.closeTaskId) {
            header = document.createElement('div');
            header.classList.add('window-header');

            label = document.createElement('span');
            label.innerHTML = me.definition.title || '';
            header.appendChild(label);

            if(me.definition.closeTaskId) {
                closeLink = document.createElement('a');
                closeLink.innerHTML = 'x';
                closeLink.href = '#';
                closeLink.classList.add('windowclose');
                closeLink.addEventListener('click',function(e) {
                    itwc.controller.sendActionEvent(me.definition.closeTaskId,'Close');
                    e.preventDefault();
                },me);

                header.appendChild(closeLink);
            }

            el.appendChild(header);

            me.itemsOffset = 1;
        }
        if(me.definition.tbar) {
            el.classList.add('vcontainer');
            me.menu = new itwc.component.itwc_menubar();
            me.menu.init({xtype: "itwc_menu_bar", items: me.definition.tbar}, me);
            me.menu.render(0);
            el.appendChild(me.menu.domEl);

            me.targetEl = document.createElement('div');
            me.targetEl.style.flex = 1;
            el.appendChild(me.targetEl);
        }

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
            var value = parseInt(e.target.value);
            itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,value == -1 ? null : value);
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
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,idx);
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
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[idx,e.target.checked]);
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
        label.setAttribute('for',nodeId + "-e");

        if(option.iconCls) {
            label.classList.add(option.iconCls);
        } else {
            label.classList.add('default-' + (option.leaf ? 'leaf' : 'folder'));
        }
        label.innerHTML = option.text;
        label.addEventListener('click',function(e) {
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,["sel",option.value,true]);
        },me);
        node.appendChild(label);

        if(option.children && option.children.length) {
            childExpand = document.createElement('input');
            childExpand.type = "checkbox"
            childExpand.id = nodeId + "-e";

            if(option.expanded) {
                childExpand.checked = true;
            }
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
                itwc.controller.sendEditEvent(me.definition.taskId,me.definition.editorId,[rowIdx]);
            },me);
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

//#### CENTRAL CONTROLLER ####//
itwc.controller = function() {
    var me = this;

    //Event queue and administration
    me.taskEvents = [];
    me.nextSendEventNo = 0;
    me.flushingTaskEvents = false;
    me.refresher = null;
    me.updateSource = null;
    me.urlParameters = '';
};
itwc.controller.prototype = {

	//Tasklets & editlets
    editlets: {},
	tasklets: {},
	taskletControllers: {},
	
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
        return eventNo;
    },
    sendEditEvent: function(taskId, editorId, value) {
        var me = this;
		
		// Client side execution hook!
		var instanceNo = taskId.split("-")[0];
		if(me.taskletControllers[instanceNo] != null){
		
			controllerWrapper(
					me.taskletControllers[instanceNo].definition.controllerFunc, 
					taskId, "edit", me.nextSendEventNo++, editorId, value);
		
		} else {// Normal case (not a tasklet)		
			return me.sendEditEventDirect(taskId,editorId,value);
		}
    },
	sendEditEventDirect: function(taskId, editorId, value) {
		var me = this;
		return me.queueTaskEvent({editEvent: JSON.stringify([taskId,editorId,value])});
	},
    sendActionEvent: function(taskId, actionId) {
        var me = this;

		// Client side execution hook!
		var instanceNo = taskId.split("-")[0];
		if(me.taskletControllers[instanceNo] != null){
			
			controllerWrapper(
					me.taskletControllers[instanceNo].definition.controllerFunc, 
					taskId, "commit", me.nextSendEventNo++, actionId);
					
		} else { // Normal case (not a tasklet)		
			return me.sendActionEventDirect(taskId,actionId);
		}
    },
	sendActionEventDirect: function(taskId, actionId) {
		var me = this;
		return me.queueTaskEvent({actionEvent: JSON.stringify([taskId,actionId])});
	},
    sendFocusEvent: function(taskId) {
        var me = this;
		
		// Client side execution hook!
		var instanceNo = taskId.split("-")[0];
		if(me.taskletControllers[instanceNo] != null){
			
			controllerWrapper(
					me.taskletControllers[instanceNo].definition.controllerFunc, 
					taskId, "focus", me.nextSendEventNo++);
					
		} else { // Normal case (not a tasklet)		
			return me.sendFocusEventDirect(taskId);
		}
    },	
    sendFocusEventDirect: function(taskId) {
        var me = this;
        return me.queueTaskEvent({focusEvent: JSON.stringify(taskId)});
    },	
    flushTaskEvents: function() {
        var me = this,
            params = {},
            xhr, event;
        if(!me.flushingTaskEvents && me.taskEvents.length) {
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
            xhr.open('POST','?format=json-gui'+me.urlParameters, true);
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
        if(msg.updates) {
            me.updateUI(msg.updates);
        }
        if(msg.done) {
            me.reset();
            return;
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
        me.updateSource = new EventSource('?format=json-gui-events&session='+me.session);
        me.updateSource.addEventListener('reset', me.onResetPushEvent.bind(me), false);
        me.updateSource.addEventListener('message', me.onUpdatePushEvent.bind(me), false);
    },
    onResetPushEvent: function(e) {
        var me = this;
        me.reset();
    },
    onUpdatePushEvent: function (e) {
        var me = this;
        me.updateUI(JSON.parse(e.data));
        me.flushingTaskEvents = false;
        me.flushTaskEvents();
    },
    updateUI: function(updates,root) {
        var me = this,
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
                        me.addComponent(cmp,op.arguments[0],op.arguments[1]);
                        //Call initializion function that needs to be applied after the full component and
                        //its children are available in the DOM
                        cmp.items[op.arguments[0]].afterAdd();
                        break;
                    case 'remove':
                        me.removeComponent(cmp,op.arguments[0]);
                        break;
                    case 'replace':
                        me.removeComponent(cmp,op.arguments[0]);
                        me.addComponent(cmp,op.arguments[0],op.arguments[1]);
                        cmp.items[op.arguments[0]].afterAdd();
                        break;
                    case 'addWindow':
                        me.addWindow(cmp,op.arguments[0],op.arguments[1]);
                        itwc.WINDOWS[op.arguments[0]].afterAdd();
                        break;
                    case 'removeWindow':
                        me.removeWindow(op.arguments[0]);
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
            cmp = itwc.WINDOWS[path[1]];
            path.splice(0,2);
        } else {
            cmp = root ? root : itwc.UI;
        }
        path.forEach(function(step) {
            cmp = (step === 'm') ? cmp.menu : cmp.items[step];
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
            console.log("Unknown component type ",insertDef.xtype,insertDef);
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

        //Add menubar items the parent support that
        if(newCmp.menu) {
            newCmp.menu.definition.items.forEach(function(menuCmp,menuIdx) {
                me.addComponent(newCmp.menu,menuIdx,menuCmp);
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
        //Call pre-removal function
        parentCmp.items[removeIdx].beforeDOMRemove();

        //We need to take the number of non-child component nodes (like a header) of the parent into account
        parentCmp.targetEl.removeChild(parentCmp.targetEl.childNodes[removeIdx + (parentCmp.itemsOffset || 0)]);
        //Remove from component tree
        parentCmp.items.splice(removeIdx,1);

        if(parentCmp.afterItemRemoved) {
            parentCmp.afterItemRemoved(removeIdx);
        }
    },
    addWindow: function(viewport,winIdx,winDef) {
        var me = this,
            win;

        win = new itwc.component.itwc_window();
        win.init(winDef,viewport);
        win.render(winIdx);

        //Add window's children
        if(winDef.items) {
            winDef.items.forEach(function(childCmp,childIdx) {
                me.addComponent(win,childIdx,childCmp);
            });
        }
        //Add menu items if the window has a menu
        if(win.menu) {
            win.menu.definition.items.forEach(function(menuCmp,menuIdx) {
                me.addComponent(win.menu,menuIdx,menuCmp);
            });
        }
        itwc.WINDOWS[winIdx] = win;

        //Inject in DOM
        document.body.appendChild(win.domEl);
        //Position window
        win.domEl.style.left = ((document.body.offsetWidth / 2) - (win.domEl.offsetWidth / 2)) + 'px';
        win.domEl.style.top = ((document.body.offsetHeight / 2) - (win.domEl.offsetHeight / 2)) + 'px';
    },
    removeWindow: function(winIdx) {
        document.body.removeChild(itwc.WINDOWS[winIdx].domEl);
        itwc.WINDOWS.splice(winIdx,1);
    },
    reset: function(startInstanceNo,startInstanceUrl) {
        var me = this,
            urlSplit;

        //Check url parameters
        if((urlSplit = window.location.toString().split('?')).length == 2) {
            me.urlParameters = '&'+urlSplit[1]
        }

        //Close push source
        if(me.updateSource) {
            me.updateSource.close();
            me.updateSource = null;
        }

        ///Reset session
        me.session = itwc.START_INSTANCE_KEY || null;

        //Empty event queue
        me.taskEvents = [];
        me.nextSendEventNo = 0;
        me.flushingTaskEvents = false;
        me.refresher = null;

        //Initialize the client component tree and DOM
        itwc.DOMID = 1000;

        itwc.UI = new itwc.component.itwc_viewport();

        itwc.UI.init();
        itwc.UI.render(0);

        //Empty list of windows
        itwc.WINDOWS = [];
    },
    start: function () {
        var me = this;

        //Initialize user interface data structures and DOM
        me.reset(itwc.START_INSTANCE_NO,itwc.START_INSTANCE_URL);
        //Start a session by sending a blank event
        me.queueTaskEvent({});

        //Listen for changes in the viewport size
        window.addEventListener('resize',me.onWindowResize,me);
    }
};

//Set up a singleton controller object
itwc.controller = new itwc.controller();
itwc.global.controller = itwc.controller; //Backwards compatibility

//Start the controller when the bootstrap page has loaded
window.onload = function() {
    itwc.controller.start();
};
