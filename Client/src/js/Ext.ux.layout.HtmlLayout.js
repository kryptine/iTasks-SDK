Ext.ns('Ext.ux.layout');
Ext.ux.layout.HtmlLayout = Ext.extend(Ext.layout.ContainerLayout, {
    renderItem: function(c, position, target){
		if(c.renderTarget){
            target = Ext.DomQuery.selectNode(c.renderTarget, Ext.getDom(target));
        }else if(c.applyTarget){
            var el = Ext.DomQuery.selectNode(c.applyTarget,  Ext.getDom(target));
            if(!c.rendered){
                c.el = el;
            }
            target = el.parentNode;
        }
        if(target){
			Ext.ux.layout.HtmlLayout.superclass.renderItem.call(this, c, undefined, target);
		}
    }
});
Ext.Container.LAYOUTS['ux.html'] = Ext.ux.layout.HtmlLayout;