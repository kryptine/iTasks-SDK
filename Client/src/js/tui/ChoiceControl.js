Ext.ns('itasks.tui');

itasks.tui.ChoiceControl = Ext.extend(Ext.form.CheckboxGroup,{
	initComponent : function(){
		this.name = this.getId();
		this.listeners = {change: {fn: this.onChange, scope: this}};
		this.msgTarget = 'side';
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		itasks.tui.ChoiceControl.superclass.initComponent.apply(this,arguments);
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange',this.dataPath,Ext.encode(this.getValue()));
	},
	onRender: function(ct, position){
		var me = this;
		
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
			
			var isSelected = function(idx){
				for(var i=0; i<me.selection.length; i++){
					if(idx == me.selection[i]) return true;
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
		itasks.tui.ChoiceControl.superclass.onRender.call(this, ct, position);
	},
	
	afterRender : function(){
		itasks.tui.ChoiceControl.superclass.afterRender.call(this);
		
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
		
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	//buffer to prevent radio group buttons firing twice (uncheck of previous -> check of new)
	fireChecked : function() {
		if(!this.checkTask){
			this.checkTask = new Ext.util.DelayedTask(this.bufferChecked, this);
		}
		this.checkTask.delay(10);
	},
	bufferChecked : function(){
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
		var sel = Ext.decode(sel);
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
	},
	setError: function(msg){		
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	}
});

Ext.reg('itasks.tui.Choice', itasks.tui.ChoiceControl);