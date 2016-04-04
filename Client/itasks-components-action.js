itasks.itwc_actionbutton = {

	domTag: 'a',
	cssCls: 'button',
	container: false,
	height: 'wrap',
	width: 'wrap',

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
				me.doActionEvent(me.taskId,me.actionId);
            }
			e.preventDefault();
			return false;
		});
    },
	onAttributeChange: function(name,value) {
		var me = this;
		switch(name) {
			case 'enabled':
				me.enabled = value;
				if(value) {
					me.domEl.classList.remove(me.cssPrefix + 'button-disabled');
				} else {
					me.domEl.classList.add(me.cssPrefix + 'button-disabled');
				}
				break;
		}
	}
};
itasks.itwc_menubutton = {
	cssCls: 'menu-item',
    initDOMEl: function() {
        var me = this,
            el = me.domEl,
            linkEl, menuEl;

        //Menu button
        linkEl = document.createElement('a');
        linkEl.href = '#';
        linkEl.innerHTML = me.text;
        el.appendChild(linkEl)

        //Menu items
		/*
        me.menu = new itwc.component.itwc_menu();
        me.menu.init({xtype: 'itwc_menu', items: me.definition.menu},me);
        me.menu.render(0,false);
        el.appendChild(me.menu.domEl);
		*/
    }
};
