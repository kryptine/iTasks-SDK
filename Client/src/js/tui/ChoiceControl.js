Ext.ns('itasks.tui');

itasks.tui.ChoiceControl = Ext.extend(Ext.form.CheckboxGroup,{

	blankText : 'Please select at least one option',

	initComponent : function(){
		if(this.staticDisplay) {
			//...
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		itasks.tui.ChoiceControl.superclass.initComponent.call(this);
	},
	
	onRender: function(ct, position){
		var me = this;
		
		
		if(!this.el && !this.staticDisplay){
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
	}
});

Ext.reg('itasks.tui.Choice', itasks.tui.ChoiceControl);