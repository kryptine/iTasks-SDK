/**
* Tab panel which shows a dashboard style 'home' screen.
*/

Ext.ns("itasks");

itasks.HomeTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: "Start",
			closable: true,
			iconCls: "icon-home",
			unstyled: true,
			cls: 'homePanel',
			autoHeight: true,
			bodyCssClass: 'homePanel-body',
			//autoLoad: "skins/" + itasks.config.skin + "/welcome.html"
			html:
				'<div class="homePanel-logo"></div><div class="homePanel-text"><h1>'+itasks.app.application+'</h1>'+
				'<p>Congratulations! You are now running the <span style="font-weight: bold">'+itasks.app.application+'</span>-application.'+
				'<p>Tasks which you can start are displayed on the left of the screen. '+
				'Clicking on such a task will start it and work will be assigned to the users. On top of the screen an overview is given of all work you are requested to complete. ' +
				'Clicking on an item at the top of the screen will open a tab in this window, which allows you to fulfill the selected assignment.'+
				'<h2>Powered by iTasks</h2>'+
				'<p>This application is powered by the iTask system. This system is developed by the <a href="http://wiki.clean.cs.ru.nl" target="_blank">Clean Team</a> in '+
				'the context of several research projects on functional specifications of workflows. '+
				'<p>To keep up with the latest developments, more information can be found on the Clean Wiki: <br />'+
				'<a href="http://wiki.clean.cs.ru.nl/ITasks" target="_blank">http://wiki.clean.cs.ru.nl/ITasks</a>'+
				'</div>'
				
		});
		
		itasks.HomeTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg("itasks.hometab",itasks.HomeTabPanel);