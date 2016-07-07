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

itasks.RadioGroup = {
	domTag: 'ul',
	cssCls: 'choice-radiogroup',
	initDOMEl: function() {
		var me = this,
			el = me.domEl,
			inputName = "choice-" + me.taskId + "-" + me.editorId,
			value = me.value.length ? me.value[0] : null;

		me.options.forEach(function(option,idx) {
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
				me.doEditEvent(me.taskId,me.editorId,idx);
            });
			liEl.appendChild(inputEl);

			labelEl = document.createElement('label');
			labelEl.setAttribute('for',inputName + "-option-" + idx);
			labelEl.innerHTML = option;
			liEl.appendChild(labelEl);

            el.appendChild(liEl);
        });
    },
    onAttributeChange: function(name,value) {
		console.log(name,value);
	}
};

itasks.CheckboxGroup = {
    domTag: 'ul',
	cssCls: 'choice-checkboxgroup',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            inputName = "choice-" + me.taskId + "-" + me.editorId,
            value = me.value || [];

        me.options.forEach(function(option,idx) {
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
                me.doEditEvent(me.taskId,me.editorId,[idx,e.target.checked]);
            });
            liEl.appendChild(inputEl);

            labelEl = document.createElement('label');
            labelEl.setAttribute('for',inputName + "-option-" + idx);
            labelEl.innerHTML = option;
            liEl.appendChild(labelEl);

            el.appendChild(liEl);
        });
    }
};

itasks.Grid = {
	cssCls: 'choicegrid',
	width: 'flex',
	height: 'flex',

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
				me.setValue([rowIdx]);
                me.doEditEvent(me.taskId,me.editorId,[rowIdx]);
            },me);
            if(me.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
					me.setValue([rowIdx]);
                    me.doEditEvent(me.taskId,me.editorId,[rowIdx]);
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
    },
	onAttributeChange: function(name,value) {
        var me = this;

		if(name == 'value') {
			me.setValue(value);
		}
    },
	setValue: function (value) {
        var me = this,
            bodyEl = me.bodyEl;

		//Remove old selection
		me.value.forEach(function(selectedIdx) {
        	bodyEl.childNodes[selectedIdx].classList.remove(me.cssPrefix + 'selected');
		});
		//Indicate new selection
		me.value = value;
		me.value.forEach(function(selectedIdx) {
			bodyEl.childNodes[selectedIdx].classList.add(me.cssPrefix + 'selected');
		});
	}
};

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
itasks.ChoiceList = {
	cssCls: 'choice-list',
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
				me.showSelection(idx);
                me.doEditEvent(me.taskId,me.editorId,idx);
            });
            optionEl.innerHTML = option;

            el.appendChild(optionEl);
        });
    },
	showSelection: function(idx) {
		var me = this,
			el = me.domEl,
			num = el.children.length, i;
			for(i = 0; i < num; i++) {
				if(i == idx) {
                	el.children[i].classList.add(me.cssPrefix + 'selected');
				} else {
                	el.children[i].classList.remove(me.cssPrefix + 'selected');
				}
			}
	}
};

