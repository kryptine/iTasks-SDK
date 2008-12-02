/**
 * Specialized window component for iTasks login
 */

Ext.ns('itasks');

itasks.LoginWindow = Ext.extend(Ext.Window, {
	
	//Initial error message
	errorMsg: "",
	
	//Initializion function
	initComponent: function() {	
		
		//PRIVATE EVENT HANDLERS
		var submitHandler = function() {
			this.getComponent(0).getForm().submit({waitMsg: 'Validating username and password...'});
		};
		var successHandler = function(form, action) {
			//Clear the error message
			this.getComponent(0).getComponent(0).setText('');
			//this.errorLabel.setText('');


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
			if(action.failureType == undefined) {
				this.getComponent(0).getComponent(0).setText(action.result.error);	
			} else {
				this.getComponent(0).getComponent(0).setText("Could not connect to server");
			}
			this.getEl().frame('#ff0000');

			//Focus the username
			form.findField('username').focus(true,false);
		};

		//CONSTRUCTION OF THE COMPONENT
		
		//Construct the login window
		Ext.apply(this, {
			title: 'Login to iTasks',
			y: 150,
			width: 350,
			height: 165,
			layout: 'fit',
			hidden: true,
			closable: false,
			resizable: false,
			items: {
				xtype: 'form',
				url: 'handlers/authenticate',
				baseCls: 'x-plain',
				cls: 'loginWindow',
				layout: 'absolute',
				defaultType: 'textfield',
				buttonAlign: 'right',
				waitMsgTarget: true,
				items: [{
							x: 55,
							y: 5,
							xtype: 'label',
							html: this.errorMsg,
							style: 'color: red; font-weight: bold'
						},{
							x: 0,
							y: 35,
							xtype: 'label',
							text: 'Username:'
						},{
							x: 55,
							y: 30,
							name: 'username',
							anchor: '100%'
						},{
							x: 0,
							y: 65,
							xtype: 'label',
							text: 'Password:'
						},{
							x: 55,
							y: 60,
							name: 'password',
							anchor: '100%',
							inputType: 'password'
				}],
				buttons: [{
					text: 'Log in',
					handler: submitHandler,
					scope: this
				}]
			},
			tools: [{
				id: 'help',
				handler: this.showHelp,
				scope: this	
			}]
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
	showHelp: function () {
		Ext.Msg.show({
			title: 'Help',
			msg: 'In this development version you can use any of the following usernames:<br />president, manager, worker1 or worker2.<br />You don\'t have to enter a password.',
			icon: Ext.MessageBox.INFO,
			buttons: Ext.Msg.OK,
			modal: false,
			animEl: this.getEl()
		});
	},
	focus: function() {
		this.getComponent(0).getForm().findField('username').focus();
	},
	continuation: function(displayName, sessionId) {
	}
	
});
