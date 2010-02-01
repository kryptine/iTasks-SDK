Ext.ns('itasks');

itasks.HorizontalGridLayout = Ext.extend(Ext.layout.ContainerLayout, {

	monitorResize: true,

	columns: 4,
	row: 0,
	column: 0,
	
	prevCell: null,

	tableAttrs:null,
	
	onLayout : function(ct, target){
		this.row = 0;
		this.column = 0;
		
		if(!this.table){
			this.table = target.createChild(Ext.apply({tag: 'table', cls: 'x-hgrid-layout-table', cellspacing: 2, cn: { tag: 'tbody'}}, this.tableAttrs),null,true);
		}
		this.renderAll(ct,target);
				
		var p = this.columns-this.column;
		var row = this.getRow(this.row);
		
		this.pad(p,row);
    },
	
	pad: function(n,row){
		for(var i=0; i<n;i++){		
			var td = document.createElement('td');
			td.width = this.getRenderWidth(1);
			row.appendChild(td);
		}	
	},
	
	getRow: function(index){
		var row = this.table.tBodies[0].childNodes[index];
		
		if(!row) {
			row = document.createElement('tr');
			this.table.tBodies[0].appendChild(row);
		}
		
		return row;
	},
	
	getRenderWidth: function(rh){
		return Math.floor(100/this.columns)*rh+'%';
	},
	
	getCell : function(c){
		
		var rh = c.renderingHint;
		
		if(this.column + rh > this.columns){	
			var pad = (this.columns - this.column);
			var row = this.getRow(this.row);
			
			this.pad(pad,row);
		
			this.row += 1;
			this.column = 0;		
		}

		var row = this.getRow(this.row);
		var td = document.createElement('td');
		
		if(c.cellId) td.id = c.cellId;
		
		var cls = 'x-hgrid-layout-cell';
		if(c.cellCls) cls += ' '+c.cellCls;
		
		td.className = cls;
		td.colSpan = rh;
		
		row.appendChild(td);
		
		this.prevCell = td;		
		this.column += rh;
		
		td.width = this.getRenderWidth(rh);
				
		return td;
	},
	
	renderItem : function(c, position, target){
				
		if(c && !c.rendered){
			c.render(this.getCell(c));
			this.configureItem(c,position);
		}else if(c && !this.isValidParent(c,target)){
			var container = this.getCell(c);
			container.insertBefore(c.getPositionEl().dom,null);
			c.container = Ext.get(container);
			this.configureItem(c, position);
		}	
		
		if(c.footer){
			if(!c.buttons || c.buttons.length == 0){
				c.footer.setSize(0,0);
				c.footer.setVisible(false);
			}else{
				c.footer.setWidth(this.getRenderWidth(c.renderingHint));
			}
		}
	},
	
	isValidParent : function(c, target){
        return c.getPositionEl().up('table', 5).dom.parentNode === (target.dom || target);
    }
});
Ext.Container.LAYOUTS['itasks.hgrid'] = itasks.HorizontalGridLayout;