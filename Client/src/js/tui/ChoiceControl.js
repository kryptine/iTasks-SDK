Ext.ns('itasks.tui');

itasks.tui.ChoiceControl = itasks.tui.extendBase(Ext.form.CheckboxGroup,{
	initComponent : function(){
		// names of checkbox groups have to be unique, so use data path field for events
		this.dataPath = this.name;
		this.name = this.getId();
		
		itasks.tui.base.initComponent.call(this,arguments);
	},
	onChange: function() {
		this.fireEvent('tuichange',this.taskId,this.dataPath,this.getValue());
	},
	onRender: function(ct, position){
		if(!this.el){
			var panelCfg = {
				autoEl : { id: this.id },
				renderTo: ct,
				bufferResize: false,
				layout: 'auto',
				cls: (this.allowMultiple)?'x-form-check-group':'x-form-radio-group',
				style: 'border: none'
			};
			
			var items = [];
			var selection = this.value;
			var isSelected = function(idx){
				for(var i=0; i<selection.length; i++){
					if(idx == selection[i]) return true;
				}
				
				return false;
			};
			
			//build the subitems based on 'allowMultiple': true = checkboxes / false = radiobuttons
			for(var i=0; i < this.options.length; i++){
				if(this.allowMultiple){
					items.push({
						xtype: 'checkbox',
						boxLabel: this.options[i],
						id: this.id+'-cb-'+i,
						name: 'sel-'+i,
						value: i,
						checked: isSelected(i)
					});
				}else{
					items.push({
						xtype: 'radio',
						boxLabel: this.options[i],
						name: this.name,
						value: i,
						checked: isSelected(i)
					});
				}
			}
		
			Ext.apply(panelCfg,{
				items: items,
				defaults: this.defaults
			});
		
			this.panel = new Ext.Container(panelCfg);
			this.panel.ownerCt = this;
			this.el = this.panel.getEl();
			
			var fields = this.panel.findBy(function(c){
				return c.isFormField;
			}, this);
			
			this.items = new Ext.util.MixedCollection();
			this.items.addAll(fields);
		}
		this.extSuperclass.onRender.call(this, ct, position);
	},
	
	afterRender : function(){
		itasks.tui.base.afterRender.call(this);
		
		this.eachItem(function(item){
			item.on('check',this.fireChecked, this);
			item.inGroup = true;
		});
		
		// determine max width of items used to align hint/error icon
		var maxWidth = 0;
		this.items.each(function(item) {
			var el = item.getEl();
			var width = el.getWidth() + el.next().getWidth();
			if(width > maxWidth)
				maxWidth = width;
		});

		this.customIconAlign = {
			el: this.el,
			position: 'l-l',
			offsets: [maxWidth + 10,0]
		};
	},
	fireChecked : function() {
		var arr = [];
		this.eachItem(function(item){
			if(item.checked){
				arr.push(item);
			}
		});
		this.fireEvent('change', this, arr);
	},
	doLayout: function(){
		if(this.rendered){
			this.panel.forceLayout = this.ownerCt.forceLayout;
			this.panel.doLayout();
		}
	},
	//returns a list of checked indices	
	getValue : function(){	
		var out = [];
		var multiple = this.allowMultiple;
			
		this.eachItem(function(item){
			if(item.checked){
				out.push(item.value);
				if(!multiple) return false;
			}
		});
				
		return out;
	},
	setValue : function(sel){
		if(Ext.isArray(sel)){
			var selection = new Ext.util.MixedCollection();
			selection.addAll(sel);
			for(var i=0; i<this.items.getCount(); i++){
				if(selection.contains(i)){
					this.items.get(i).setValue(true);
					if(!this.allowMultiple) break;
				}else{
					this.items.get(i).setValue(false);
				}
			}
		}
	}
});

Ext.reg('itasks.tui.Choice', itasks.tui.ChoiceControl);