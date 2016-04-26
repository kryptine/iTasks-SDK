itasks.itwc_choice_grid = {
	cssCls: 'choicegrid',
	width: 'flex',
	container: false,	

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
                me.doEditEvent(me.taskId,me.editorId,[rowIdx]);
            },me);
            if(me.doubleClickAction) {
                rowEl.addEventListener('dblclick',function(e) {
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
        var me = this,
            bodyEl = me.bodyEl;
		if(name == 'value') {

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
    }
};

itasks.itwc_choice_tree = {
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
                me.doEditEvent(me.taskId,me.editorId,["sel",option.value,true]);
        },me);

        if(me.doubleClickAction) {
            label.addEventListener('dblclick',function(e) {
                me.doEditEvent(me.taskId,me.editorId,["sel",option.value,true]);
                me.doActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);

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
itasks.itwc_choice_list = {
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
                me.doEditEvent(me.taskId,me.editorId,idx);
            });
            optionEl.innerHTML = option;

            el.appendChild(optionEl);
        });
    }
};


