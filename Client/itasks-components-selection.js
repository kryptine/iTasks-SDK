itasks.Dropdown = {
    domTag: 'select',
    width: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            value = me.value[0],
            option;

        option = document.createElement('option');
        option.innerHTML = "Select...";
        option.value = -1;
        el.appendChild(option);

        me.options.forEach(function(label,index) {
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
            me.doEditEvent(me.taskId,me.editorId,value == -1 ? null : value,false);
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
};

itasks.ListSelect = { //Mixin for selections in lists based on index
	select: function (selection, toggle = false) {
        var me = this,
			options = me.optionsEl.children,
			numOptions = options.length,
			oldSelection = me.value.slice(0),
			i;

		if(toggle) {
 			//Unselect items in the toggle set
			me.value = me.value.filter(function(x) {return !selection.includes(x)});
			//Add the items from the selection that were not already selected
			me.value = me.value.concat(selection.filter(function(x) {return !oldSelection.includes(x)}));
		} else {
			me.value = selection;
		}
		//Update DOM
		for(i = 0; i < numOptions; i++) {
			me.selectInDOM(options[i],me.value.includes(i));
		}
	},
    onAttributeChange: function(name,value) {
		if(name == 'value') {
			me.select(value,false);
		}
	}
};

itasks.CheckGroup = Object.assign({
	domTag: 'ul',
	cssCls: 'checkgroup',
	multiple: false,
	initDOMEl: function() {
		var me = this,
			el = me.domEl,
			inputName = "choice-" + me.taskId + "-" + me.editorId,
			value = me.value.length ? me.value[0] : null;

		me.options.forEach(function(option,idx) {
			var liEl,inputEl,labelEl;
			liEl = document.createElement('li');
			inputEl = document.createElement('input');
			inputEl.type = me.multiple ? 'checkbox' : 'radio';
			inputEl.value = idx;
			inputEl.name = inputName;
			inputEl.id = inputName + "-option-" + idx;
			if(idx === value) {
				inputEl.checked = true;
            }
            inputEl.addEventListener('click',function(e) {
				me.select([idx],me.multiple);
				me.doEditEvent(me.taskId,me.editorId,me.value);
            });
			liEl.appendChild(inputEl);

			labelEl = document.createElement('label');
			labelEl.setAttribute('for',inputName + "-option-" + idx);
			labelEl.innerHTML = option;
			liEl.appendChild(labelEl);

            el.appendChild(liEl);
        });
		me.optionsEl = me.domEl;
    },
	selectInDOM: function(el,selected) {
		el.children[0].checked = selected;
	}
},itasks.ListSelect);

itasks.ChoiceList = Object.assign({
	cssCls: 'choice-list',
	multiple: false,
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            value = me.value.length ? me.value[0] : null;

        me.options.forEach(function(option,idx) {
            var optionEl;
            optionEl = document.createElement('div');
            optionEl.classList.add(me.cssPrefix + 'choice-list-option');
            if(idx === value) {
                optionEl.classList.add(me.cssPrefix + 'selected');
            }
            optionEl.addEventListener('click',function(e) {
				me.select([idx], me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
				e.preventDefault();
            });
            optionEl.innerHTML = option;

            el.appendChild(optionEl);
        });
		me.optionsEl = me.domEl;
    },
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}

},itasks.ListSelect);

itasks.Grid = Object.assign({
	cssCls: 'choicegrid',
	width: 'flex',
	height: 'flex',
	multiple: false,

    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            headerEl,bodyEl,rowEl,cellEl;

        //Create header
        headerEl = me.headerEl = document.createElement('div');
        headerEl.classList.add(me.cssPrefix + 'choicegrid-header');
        me.columns.forEach(function(column) {
            cellEl = document.createElement('div');
            cellEl.innerHTML = column;
            headerEl.appendChild(cellEl);
        });
        el.appendChild(headerEl);

        //Create body
        bodyEl = me.bodyEl = document.createElement('div');
        bodyEl.classList.add(me.cssPrefix + 'choicegrid-body');
        me.options.forEach(function(option,rowIdx) {
            rowEl = document.createElement('div');
            rowEl.addEventListener('click',function(e) {
				me.select([rowIdx], me.multiple && (e.metaKey || e.ctrlKey));
                me.doEditEvent(me.taskId,me.editorId,me.value);
            },me);
            if(me.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
					me.select([rowIdx]);
                    me.doEditEvent(me.taskId,me.editorId,me.value);
                    me.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);

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
        if(me.value.length) {
            me.value.forEach(function(selectedIdx) {
                bodyEl.childNodes[selectedIdx].classList.add(me.cssPrefix + 'selected');
            });
        }
        el.appendChild(bodyEl);
		me.optionsEl = me.bodyEl;
    },
	initContainerEl: function() {},
	selectInDOM(el,selected) {
		el.classList[selected ? 'add':'remove'](this.cssPrefix + 'selected');
	}
},itasks.ListSelect);

itasks.Tree = {
    height: 'flex',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            rootNodeId = me.taskId+ "-" + me.editorId + "-node",
            rootNode,node;

        el.classList.add(me.cssPrefix + 'choicetree');

        rootNode = document.createElement('ol');

        //Create a table for quick access
        me.selection = me.value || [];
        me.nodes = [];

        me.options.forEach(function(option,idx) {
            me.addNode(option,rootNode,rootNodeId,idx);
        },me);

        me.selection.forEach(function(idx) {
            me.nodes[idx].classList.add(me.cssPrefix + 'selected');
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
            node.classList.add(me.cssPrefix + 'leaf');
        }
        label = document.createElement('label');
        label.id = nodeId + "-l";

        if(option.iconCls) {
            label.classList.add(option.iconCls);
        } else {
            label.classList.add(me.cssPrefix + 'default-' + (option.leaf ? 'leaf' : 'folder'));
        }
        label.innerHTML = option.text;
        label.addEventListener('click',function(e) {
				me.setValue([option.value]);
                me.doEditEvent(me.taskId,me.editorId,["sel",option.value,true]);
        },me);

        if(me.doubleClickAction) {
            label.addEventListener('dblclick',function(e) {
				me.setValue([option.value]);
                me.doEditEvent(me.taskId,me.editorId,["sel",option.value,true]);
                me.doEditEvent(me.doubleClickAction[0],null,me.doubleClickAction[1]);

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
                me.doEditEvent(me.taskId,me.editorId,["exp",option.value,childExpand.checked]);
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
            me.nodes[idx].classList.remove(me.cssPrefix + 'selected');
        });
        me.selection = value;
        me.selection.forEach(function(idx) {
            me.nodes[idx].classList.add(me.cssPrefix + 'selected');
        });
    }
};

