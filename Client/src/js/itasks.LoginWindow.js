/**
 * Specialized window component for iTasks login
 */

Ext.ns('itasks');

itasks.LoginWindow = Ext.extend(Ext.Window, {
	
	//Initial error message
	errorMsg: "",
	
	//Initializion function
	initComponent: function() {	
		
		var submitHandler = function() {
			this.getComponent(0).getForm().submit({waitMsg: 'Validating username and password...'});
		};
		var successHandler = function(form, action) {
			//Clear the error message
			this.getComponent(0).getComponent(0).setText('');

			//Fade out the window
			this.getEl().fadeOut({
				callback: function() {
					this.continuation(action.result.displayName, action.result.sessionId);		
				},
				scope: this
			});
		};
		var failureHandler = function(form, action) {
			//Show the error and draw attention to the window
			
			if(action.failureType == "connect") {
				this.getComponent(0).getComponent(1).setText("Could not connect to server");
			} else {
				this.getComponent(0).getComponent(1).setText(action.result.error);	
			}
			this.getEl().frame('#ff0000');

			//Focus the username
			form.findField('username').focus(true,false);
		};
		
		Ext.apply(this, {
			y: 150,
			width: 320,
			height: 185,
			layout: "fit",
			hidden: true,
			closable: false,
			resizable: false,
			shadow: false,
			items: {
				xtype: 'form',
				url: itasks.config.serverUrl + '/authenticate',
				baseCls: 'x-plain',
				style: 'padding: 5px',
				layout: 'absolute',
				defaultType: 'textfield',
				buttonAlign: 'right',
				waitMsgTarget: true,
				items: [{
							x: 0,
							y: 0,
							xtype: 'label',
							style: "color: red; font-weight: bold; background: url('skins/" + itasks.config.skin + "/img/loginwindow.png')",
							width: 300,
							height: 40
						},{
							x: 55,
							y: 45,
							xtype: 'label',
							html: this.errorMsg,
							style: "color: red; font-weight: bold;"
						},{
							x: 0,
							y: 70,
							xtype: 'label',
							text: 'Username:'
						},{
							x: 55,
							y: 65,
							anchor: '100%',
							name: 'username'
						},{
							x: 0,
							y: 100,
							xtype: 'label',
							text: 'Password:'
						},{
							x: 55,
							y: 95,
							anchor: '100%',
							name: 'password',
							inputType: 'password'
				}],
				buttons: [{
					text: 'Log in',
					handler: submitHandler,
					scope: this
				}]
			}
		});

		//initialize the superclass (Ext.Window)
		itasks.LoginWindow.superclass.initComponent.apply(this, arguments);
		
		var loginPanel = this.getComponent(0);
	
		//Attach the success and failure event handlers
		loginPanel.on('actioncomplete', successHandler, this);
		loginPanel.on('actionfailed', failureHandler, this);

		//Add a keymap to connect <enter> in the form to the submit action
		loginPanel.on('render', function () {
			new Ext.KeyMap(loginPanel.getEl(), {
				key: Ext.EventObject.ENTER,
				fn: submitHandler,
				scope: this
			});
		},this);
	},
	focus: function() {
		this.getComponent(0).getForm().findField('username').focus();
	},
	setError: function(msg) {
		this.getComponent(0).getComponent(1).setText(msg ? msg : "");	
	},
	continuation: function(displayName, sessionId) {
	}
	
});
