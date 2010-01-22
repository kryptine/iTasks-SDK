Ext.ns("itasks");

itasks.ListPanel = Ext.extend(Ext.Panel,
{
	
	selectedItems : {},
	
	initComponent: function(){
				
		Ext.apply(this,
		{ autoHeight: true
		, border: false
		, cls: 'list'
		});	
		
		itasks.ListPanel.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(arguments){		
		//in case of an empty list, add an 'add'-button.
		if(!this.items || this.items.length == 0){
		
			var addButton = new Ext.Button({
				text: 'Add Element',
				iconCls: 'list-button-add',
				renderTarget: 'span[name=button]'
			});
		
			var addButtonPanel = new Ext.Panel({
				layout: 'ux.html',
				html: '<table><tr><td style="font-style: italic; padding-right: 5px;">The list is empty.</td><td><span name="button"></span></td></tr></table>',
				items: [addButton],
				baseCls: 'list-item'
			});
			
			var addFunct = function(){
				var formCt = this.findParentByType('itasks.task-form');
				formCt.addUpdate(this.name,"add_-1");
				formCt.sendUpdates(false);	
			}
			
			addButton.on('click',addFunct.createDelegate(this));
		
			this.add(addButtonPanel);
		}
		
		itasks.ListPanel.superclass.afterRender.call(this,arguments);
	}
});

Ext.ns("itasks.list");

itasks.list.Button = Ext.extend(Ext.Button,
{
	onRender: function(ct,position){
		
        var t = new Ext.Template('<div id="{1}" class="list-toolbox-button {2}"><button type="{0}"></button></div>');
        t.compile();

        this.template = t;
        var btn, targs = this.getTemplateArgs();

        if(position){
			btn = this.template.insertBefore(position, [this.type,this.id,this.btnCls], true);
        }else{
			btn = this.template.append(ct,[this.type,this.id,this.btnCls], true);
        }

        this.btnEl = btn.child(this.buttonSelector);
        this.mon(this.btnEl, {
            scope: this,
            focus: this.onFocus,
            blur: this.onBlur
        });

        this.initButtonEl(btn, this.btnEl);
        Ext.ButtonToggleMgr.register(this);		
	}
});

itasks.list.Toolbox = Ext.extend(Ext.Panel,
{
	initComponent: function(){
		this.upButton   = new itasks.list.Button({btnCls: 'list-button-up'});
		this.downButton = new itasks.list.Button({btnCls: 'list-button-down'});
		this.addButton  = new itasks.list.Button({btnCls: 'list-button-add'});
		this.remButton  = new itasks.list.Button({btnCls: 'list-button-rem'});
		
		Ext.apply(this,
		{ width: 70
		, layout: 'hbox'
		, items: [this.upButton,this.downButton,this.addButton,this.remButton]
		, cls: 'list-toolbox'
		, unstyled: true
		});
		
		itasks.list.Toolbox.superclass.initComponent.apply(this,arguments);
	},
	
	handleClick: function(action,name,index){
		var formCt = this.findParentByType('itasks.task-form');
		formCt.addUpdate(name,action+"_"+index);
		formCt.sendUpdates(false);	
	},
	
	afterRender: function(arguments){
		this.upButton.on('click',this.handleClick.createDelegate(this,['mup',this.name,this.index]));
		this.downButton.on('click',this.handleClick.createDelegate(this,['mdn',this.name,this.index]));
		this.addButton.on('click',this.handleClick.createDelegate(this,['add',this.name,this.index]));
		this.remButton.on('click',this.handleClick.createDelegate(this,['rem',this.name,this.index]));
	
		itasks.list.Toolbox.superclass.afterRender.call(this,arguments);
	}
});

itasks.list.Item = Ext.extend(Ext.Panel,
{	
	initComponent: function(){
		
		Ext.apply(this,
		{ border: false
		, cls: 'list-item'
		, autoHeight: true
		, unstyled: true
		});
		
		itasks.list.Item.superclass.initComponent.apply(this,arguments);
	},

	afterRender: function(arguments){		
		this.toolbox = new itasks.list.Toolbox({index: this.index, name: this.name})
		this.add(this.toolbox);
		
		itasks.list.Item.superclass.afterRender.call(this,arguments);	
	}
});

Ext.reg("itasks.list",itasks.ListPanel);
Ext.reg("itasks.list.item",itasks.list.Item);
Ext.reg("itasks.list.tool",itasks.list.Toolbox);
Ext.reg("itasks.list.button",itasks.list.Button);