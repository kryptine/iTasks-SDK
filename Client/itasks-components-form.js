itasks.itwc_label = {
    domTag: 'label',
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.innerHTML = me.text;
    }
};
itasks.itwc_view_icon = {

	width: 'wrap',
	height: 'wrap',
	initDOMEl: function() {
		var me = this,
			el = me.domEl;
		el.classList.add(me.cssPrefix + 'icon');
		el.classList.add(me.iconCls);
		me.currentIcon = me.iconCls;

		if(me.tooltip) {
			el.setAttribute('tooltip',me.tooltip);
		}
    },
	onAttributeChange: function(name,value) {
		var me = this,
			el = me.domEl;
		switch(name) {
			case 'iconCls':
				el.classList.remove(me.currentIcon);
				me.currentIcon = value;
				el.classList.add(me.currentIcon);
				break;
			case 'tooltip':
				el.setAttribute('tooltip',value);
				break;
		}
	}
};
itasks.itwc_edit_string = {
	domTag: 'input',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'text';
		el.value = me.value ? me.value : '';
		el.addEventListener('keyup',function(e) {
            var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
		});
	},
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.itwc_edit_note = {
    domTag: 'textarea',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.innerHTML = me.value ? me.value : '';
        el.addEventListener('keyup',function(e) {
			var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
        });
    }
};
itasks.itwc_edit_password = {
	domTag: 'input',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'password';
		el.value = me.value ? me.value : '';
		el.addEventListener('keyup',function(e) {
            var value = e.target.value === "" ? null : e.target.value
			me.doEditEvent(me.taskId,me.editorId,value);
		});
	}
};
itasks.NumberField = {
	domTag: 'input',
    allowDecimal: false,
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
		el.value = (me.value === undefined || me.value === null) ? '' : me.value;

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
            me.doEditEvent(me.taskId,me.editorId,value);
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
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};

itasks.itwc_edit_int = Object.assign(itasks.NumberField,{
    allowDecimal: false
});
itasks.itwc_edit_decimal = Object.assign(itasks.NumberField,{
    allowDecimal: true
});

itasks.itwc_edit_checkbox = {
	domTag: 'input',
    defaultWidth: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'checkbox';
        el.checked = me.value;

        el.addEventListener('click',function(e) {
			var value = e.target.checked;
            me.doEditEvent(me.taskId,me.editorId,value);
        });
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
        	me.domEl.checked = value;
		}
	}
};

itasks.itwc_edit_slider = {
	domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        el.min = me.minValue;
        el.max = me.maxValue;
        el.value = me.value;

        el.addEventListener('change',function(e) {
            me.doEditEvent(me.taskId,me.editorId, (e.target.value | 0),true);
        });
    }
};

itasks.itwc_edit_date = {
	domTag: 'input',
    width: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.value ? me.value : '';
        el.addEventListener('keyup',function(e) {
            me.doEditEvent(me.taskId,me.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    }
};
itasks.itwc_edit_time = {
	domTag: 'input',
    width: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.value ? me.value : '';
        el.addEventListener('keyup',function(e) {
            me.doEditEvent(me.taskId,me.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    }
};

itasks.itwc_edit_datetime = {
	domTag: 'input',
    width: 'wrap',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
        el.value = me.value ? me.value : '';
        el.addEventListener('keyup',function(e) {
            me.doEditEvent(me.taskId,me.editorId,e.target.value === "" ? null : e.target.value,true);
        });
    }
};
itasks.itwc_edit_document = {
	cssCls: 'edit-document',
    initDOMEl: function() {

        var me = this,
            el = this.domEl;

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
        me.value = me.value || null;
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
            me.doEditEvent(me.taskId,me.editorId,null);
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
            me.sendEditEvent(me.taskId,me.editorId,rsp[0]);
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
};

itasks.itwc_editbutton = {
	domTag: 'a',
	cssCls: 'button',
	container: false,
	height: 'wrap',
	width: 'wrap',
	enabled: true,

	initDOMEl: function() {
		var me = this,
			el = me.domEl;

		el.href = '#';
		if(me.iconCls) {
			me.icon = document.createElement('div');
			me.icon.classList.add(me.cssPrefix + 'button-icon');
			me.icon.classList.add(me.iconCls);
			el.appendChild(me.icon);
		}
		if(!me.enabled) {
			el.classList.add(me.cssPrefix + 'button-disabled');
		}
		if(me.text) {
			me.label = document.createElement('div');
			me.label.innerHTML = me.text;
			me.label.classList.add(me.cssPrefix + 'button-label');
			el.appendChild(me.label);
		}
        el.addEventListener('click',function(e) {
            if(me.enabled) {
				me.doEditEvent(me.taskId,me.editorId,me.value);
            }
			e.preventDefault();
			return false;
		});
    }
};

itasks.itwc_choice_dropdown = {
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

itasks.itwc_choice_radiogroup = {
	domTag: 'ul',
	cssCls: 'choice-radiogroup',
	container: false,
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

itasks.itwc_choice_checkboxgroup = {
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


