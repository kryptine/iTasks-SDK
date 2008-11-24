/**
 * Specialized window component for iTasks login
 */

Ext.ns('itasks');

itasks.LoginWindow = Ext.extend(Ext.Window, {

	//The error display area
	errorLabel: new Ext.form.Label({
		x: 55,
		y: 5,
		xtype: 'label',
		style: 'color: red; font-weight: bold'
	}),

	//The embedded form 
	loginPanel: new Ext.form.FormPanel({
		url: 'handlers/authenticate',
		baseCls: 'x-plain',
		layout: 'absolute',
		defaultType: 'textfield',
		buttonAlign: 'right',
		waitMsgTarget: true,
		items: [{
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
				}
		]
	}),
	
	//Initializion function
	initComponent: function() {	
		
		//PRIVATE EVENT HANDLERS
		var submitHandler = function() {
			this.loginPanel.getForm().submit({waitMsg: 'Validating username and password...'});
		};
		var successHandler = function(form, action) {
			//Clear the error message
			this.errorLabel.setText('');


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
				this.errorLabel.setText(action.result.error);	
			} else {
				this.errorLabel.setText("Could not connect to server");
			}
			this.getEl().frame('#ff0000');

			//Focus the username
			form.findField('username').focus(true,false);
		};

		//CONSTRUCTION OF THE COMPONENT
		
		//Construct the login window
		Ext.apply(this, {
			id: 'loginWindow',
			title: 'Login to iTasks',
			y: 150,
			width: 350,
			height: 165,
			layout: 'fit',
			hidden: true,
			bodyStyle:'padding: 5px;',
			closable: false,
			resizable: false,
			items: this.loginPanel,
			tools: [{
				id: 'help',
				handler: this.showHelp,
				scope: this	
			}]
		});

		//initialize the superclass (Ext.Window)
		itasks.LoginWindow.superclass.initComponent.apply(this, arguments);

		//Add the error label to the form
		this.loginPanel.add(this.errorLabel);

		//Add a submit button to the form panel to submit the form
		this.loginPanel.addButton({
			text: 'Log in',
			handler: submitHandler,
			scope: this
		});	

		//Attach the success and failure event handlers
		this.loginPanel.on('actioncomplete', successHandler, this);
		this.loginPanel.on('actionfailed', failureHandler, this);

		//Add a keymap to connect <enter> in the form to the submit action
		this.loginPanel.on('render', function () {
			new Ext.KeyMap(this.loginPanel.getEl(), {
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
		this.loginPanel.getForm().findField('username').focus();
	},
	continuation: function(displayName, sessionId) {
	}
	
});
