//Abstract class containing the selection/toggle behavior
itasks.Selector = class extends itasks.Component {
	select(selection, toggle = false) {
		var me = this,
			options = me.attributes.options,
			oldSelection = me.attributes.value.slice(0),
			i;
		if(toggle) {
			//Unselect items in the toggle set
			me.attributes.value = me.attributes.value.filter(function(x) {return !selection.includes(x)});
			//Add the items from the selection that were not already selected
			me.attributes.value = me.attributes.value.concat(selection.filter(function(x) {return !oldSelection.includes(x)}));
		} else {
			me.attributes.value = selection;
		}
		//Update DOM
		options.forEach(me.selectOptionsInDOM.bind(me));
	}
	selectOptionsInDOM(option) {
		var me = this;
		me.selectInDOM(option.domEl,me.attributes.value.includes(option.id));
		if(option.children) {
			option.children.forEach(me.selectOptionsInDOM.bind(me));
		}
	}
	onAttributeChange(name,value) {
		var me = this;
		switch(name) {
			case 'value':
			me.select(value,false);
			break;
			case 'options':
			me.setOptions(value);
			break;
		}
	}
};

itasks.Dropdown = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
    	this.domTag = 'select';
		this.attributes = Object.assign({
			width: 150,
			height: 'wrap',
			multiple: false
		},this.attributes);
	}
	initDOMEl() {
        var me = this,
            el = me.domEl;

		if(me.attributes.multiple){
			el.multiple = true;
		}
		if (me.attributes['enabled'] === false) {
			el.disabled = true;
		}

        me.setOptions(me.attributes.options);

		//If the selection is a single one, a single handler on the select
		//suffices
		if(!me.attributes.multiple && !el.disabled){
			el.addEventListener('change',function(e) {
				me.select(e.target.value === '' ? [] : [parseInt(e.target.value)]);
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
			});
		}
	}
	selectInDOM(el,selected) {
		el.selected = selected;
	}
	setOptions(options) {
        var me = this,
            el = me.domEl;

        //Store options
		me.attributes.options = options;

        //Clear
		while (el.lastChild) {
    		el.removeChild(el.lastChild);
		}

        if(!me.attributes.multiple) {
			//The empty selection (only for single selections)
			var optionEl = document.createElement('option');
        	optionEl.innerHTML = "Select...";
        	optionEl.value = "";
        	el.appendChild(optionEl);
		}

		var curGroupLabel = null;
		var curParentEl   = el;

		options.forEach(function(option) {
			var optionEl = document.createElement('option');
            optionEl.value = option.id;
            optionEl.innerHTML = option.text;
            if(me.attributes.value.includes(option.id)) {
                me.selectInDOM(optionEl, true);
            }

            if (curGroupLabel == option.grouplabel) {
                curParentEl.appendChild(optionEl);
            } else {
                curGroupLabel = option.grouplabel;

                if (option.grouplabel) {
                    curParentEl = document.createElement('optgroup');
                    curParentEl.label = curGroupLabel;
                    el.appendChild(curParentEl);
                } else {
                    curParentEl = el;
                }
            }
            curParentEl.appendChild(optionEl);
			option.domEl = optionEl;

			//Only if the selection is a multiple selection we place handlers
			//on every element
			if(me.attributes.multiple && !el.disabled){
        		option.domEl.addEventListener('click',function(e) {
					me.select([option.id], me.attributes.multiple && (e.metaKey || e.ctrlKey));
            	    me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
					e.preventDefault();
        		});
			}
        },me);
	}
};

itasks.CheckGroup = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.domTag = 'ul';
		this.cssCls = 'checkgroup';
		this.attributes = Object.assign({
			multiple: false
		},this.attributes);
	}
	initDOMEl() {
		var me = this,
			el = me.domEl;
        me.setOptions(me.attributes.options);
        //Highlight initital selection
		me.select(me.attributes.value,false);
	}
	setOptions(options) {
        var me = this,
			el = me.domEl,
            inputName = "choice-" + me.attributes.taskId + "-" + me.attributes.editorId;
        //Store options
		me.attributes.options = options;

        //Clear
		while (el.lastChild) {
    		el.removeChild(el.lastChild);
		}

		options.forEach(function(option,idx) {
			var liEl,inputEl,labelEl;
			liEl = document.createElement('li');
			inputEl = document.createElement('input');
			if (me.attributes['enabled'] === false) {
				inputEl.disabled = true;
			}
			inputEl.type = me.attributes.multiple ? 'checkbox' : 'radio';
			inputEl.value = idx;
			inputEl.name = inputName;
			inputEl.id = inputName + "-option-" + option.id;
            inputEl.addEventListener('click',function(e) {
				me.select([option.id],me.attributes.multiple);
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
            });
			liEl.appendChild(inputEl);

            if(me.attributes.value.includes(option.id)) {
				me.selectInDOM(liEl, true);
            }

			labelEl = document.createElement('label');
			labelEl['for']=inputName + "-option-" + option.id;
			labelEl.innerHTML = option.text;
			liEl.appendChild(labelEl);

            el.appendChild(liEl);
			option.domEl = liEl;
        });
	}
	selectInDOM(el,selected) {
		el.children[0].checked = selected;
	}
};

itasks.ChoiceList = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'choice-list';
		this.attributes = Object.assign({
			multiple: false
		},this.attributes);
	}
	initDOMEl() {
        var me = this,
            el = me.domEl;

        me.setOptions(me.attributes.options);

		//Highlight initital selection
		me.select(me.attributes.value,false);
    }
	setOptions(options) {
         var me = this, el = me.domEl;
		//Store options
		me.attributes.options = options;

        //Clear
		while (el.lastChild) {
    		el.removeChild(el.lastChild);
		}

        options.forEach(function(option,idx) {
            var optionEl;
            optionEl = document.createElement('div');
            optionEl.classList.add(me.cssPrefix + 'choice-list-option');

			if (me.attributes['enabled'] !== false) {
		        optionEl.addEventListener('click',function(e) {
					me.select([option.id], me.attributes.multiple && (e.metaKey || e.ctrlKey));
		            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
					e.preventDefault();
		        });
			}
            optionEl.innerHTML = option.text;

            if(me.attributes.value.includes(option.id)) {
				me.selectInDOM(optionEl, true);
            }

            el.appendChild(optionEl);
			option.domEl = optionEl;
        });
	}
	selectInDOM(el, selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
};

itasks.Grid = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.cssCls = 'choicegrid';
		this.attributes = Object.assign({
			width: 'flex',
			height: 'flex',
			multiple: false,
			columns: []
		},this.attributes);
	}
	initDOMEl() {
        var me = this,
            el = me.domEl,
            headerEl,bodyEl,rowEl,cellEl;

        //Create header
        headerEl = me.headerEl = document.createElement('div');
        headerEl.classList.add(me.cssPrefix + 'choicegrid-header');
        me.attributes.columns.forEach(function(column) {
            cellEl = document.createElement('div');
            cellEl.innerHTML = column;
            headerEl.appendChild(cellEl);
        });
        el.appendChild(headerEl);

        //Create body
        bodyEl = me.bodyEl = document.createElement('div');
        bodyEl.classList.add(me.cssPrefix + 'choicegrid-body');

		//Fill with options
		me.setOptions(me.attributes.options);

        //Indicate initial selection
        if(me.attributes.value.length) {
            me.attributes.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add(me.cssPrefix + 'selected');
            });
        }
        el.appendChild(bodyEl);
	}
	setOptions(options) {
		var me = this, bodyEl = me.bodyEl, rowEl, cellEl;
		//Store options
		me.attributes.options = options;

		//Clear
		while (bodyEl.lastChild) {
    		bodyEl.removeChild(bodyEl.lastChild);
		}
		//Add rows
        options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');

			if (me.attributes['enabled'] !== false) {
		        rowEl.addEventListener('click',function(e) {
					me.select([option.id], me.attributes.multiple && (e.metaKey || e.ctrlKey));
		            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
		        },me);
		        if(me.attributes.doubleClickAction) {
		            rowEl.addEventListener('dblclick',function(e) {
						me.select([option.id]);
		                me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
		                me.sendActionEvent(me.attributes.doubleClickAction[0],me.attributes.doubleClickAction[1]);

		                e.stopPropagation();
		                e.preventDefault();
		            },me);
		        }
			}
            option.cells.forEach(function(cell) {
                cellEl = document.createElement('div');
                cellEl.innerHTML = cell;
                rowEl.appendChild(cellEl);
            });

            if(me.attributes.value.includes(option.id)) {
				me.selectInDOM(rowEl, true);
            }

            bodyEl.appendChild(rowEl);
			option.domEl = rowEl;
        });
	}
	initContainerEl() {
	}
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
};

itasks.Tree = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.attributes = Object.assign({
			height: 'flex',
			multiple: false,
			options: []
		},this.attributes);
	}
	initDOMEl() {
        var me = this,
            el = me.domEl;

        el.classList.add(me.cssPrefix + 'choicetree');

        me.rootNode = document.createElement('ol');
		me.rootNodeId = me.attributes.taskId + "-" + me.attributes.editorId + "-node";
        me.setOptions(me.attributes.options);
        el.appendChild(me.rootNode);
    }
    addNode(option,parentNode,rootNodeId,idx) {
        var me = this,
            node,nodeId,label,childExpand,childOl;

        nodeId = rootNodeId + "-"+ idx;
        node = document.createElement('li');
        node.id = nodeId;

        label = document.createElement('label');
        label.id = nodeId + "-l";

        if(option.iconCls) {
            label.classList.add(option.iconCls);
        } else {
            label.classList.add(me.cssPrefix + 'default-' + (option.children.length ? 'folder' : 'leaf'));
        }
        label.innerHTML = option.text == "" ? "&nbsp;" : option.text;

		if (me.attributes['enabled'] !== false) {
		    label.addEventListener('click',function(e) {
					me.select([option.id],me.attributes.multiple && (e.metaKey || e.ctrlKey));
		            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
		    },me);

		    if(me.attributes.doubleClickAction) {
		        label.addEventListener('dblclick',function(e) {
					me.select([option.id]);
		            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
		            me.doEditEvent(me.attributes.doubleClickAction[0],null,me.attributes.doubleClickAction[1]);

		            e.stopPropagation();
		            e.preventDefault();
		        });
		    }
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
				//TODO: sync node expansion with server?
                //me.doEditEvent(me.taskId,me.editorId,["exp",option.value,childExpand.checked]);
            },me);

            node.appendChild(childExpand);

            label = document.createElement('label');
            label.setAttribute('for', childExpand.id);
            node.appendChild(label);

            childOl = document.createElement('ol');
            option.children.forEach(function(option,childIdx) {
                me.addNode(option,childOl,nodeId,childIdx);
            },me);
            node.appendChild(childOl);
        }
        parentNode.appendChild(node);

        //Track the option in the dom
		option.domEl = node;
	}
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
	setOptions(options) {
		const me = this;

		//Clear
		while (me.rootNode.lastChild) {
			me.rootNode.removeChild(me.rootNode.lastChild);
		}

		//Add options
		options.forEach(function(option,idx) {
            me.addNode(option,me.rootNode,me.rootNodeId,idx);
        },me);

		//Select options
		me.select(me.attributes.value, false);
	}
};

itasks.TabBar = class extends itasks.Selector {
	constructor(spec,parentCmp) {
		super(spec,parentCmp);
		this.domTag = 'ul';
		this.cssCls = 'tabbar';
		this.attributes = Object.assign({
			height: 'wrap',
			width: 'flex',
			multiple: false
		},this.attributes);
	}
	initDOMEl() {
		var me = this;
		me.setOptions(me.attributes.options);
		me.select(me.attributes.value, false);
	}
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
	setOptions(options) {
		var me = this, el = me.domEl;

		//Store options
		me.attributes.options = options;

		//Clear
		while (el.lastChild) {
			el.removeChild(el.lastChild);
		}

		options.forEach(function(option) {
			var optionEl = document.createElement('li');
			optionEl.value = option.id;

			var label = document.createElement('a');
			label.innerHTML = '<span>'+ (option.text || '-')+'</span>';
			label.href = '#';

			label.addEventListener('click',function(e) {
				me.select([option.id], false);
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,me.attributes.value);
				e.preventDefault();
			},me);
			optionEl.appendChild(label);

			if(me.attributes.value.includes(option.id)) {
				me.selectInDOM(optionEl, true);
			}
			option.domEl = optionEl;
			el.appendChild(optionEl);
		 },me);
	}
};
