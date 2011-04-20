Ext.ns('itasks.tui');

itasks.tui.DocumentControl = itasks.tui.extendBase(Ext.Panel, {
	unstyled: true,
	width: 500,
	layout: 'card',
	activeItem: 0,
	autoHeight: true,
	initComponent : function(){
		this.listeners = {};
		
		this.uploadPanel = new itasks.tui.document.UploadPanel();
		this.downloadPanel = new itasks.tui.document.DownloadPanel();
		
		this.items = [
				this.uploadPanel,
				this.downloadPanel
		];
	
		itasks.tui.base.initComponent.apply(this,arguments);
	},
	afterRender : function(arguments){
		itasks.tui.base.afterRender.apply(this,arguments);
		
		if(this.document.size != 0){
			this.showDownloadPanel(false);
		}else{
			this.showUploadPanel(false);
		}
	},
	setValue: function(value){
		this.document = value;
		if(!this.rendered) return;
	
		if(this.document.size == 0){
			this.showUploadPanel(false);
		}else{
			this.showDownloadPanel(false);
		}
	},
	
	showDownloadPanel : function(isStatic){
		this.downloadPanel.updateDocInfo(this.document);
		this.downloadPanel.setStaticDisplay(isStatic);
		this.getLayout().setActiveItem(1);
	},
	
	showUploadPanel : function(showCancel){
		this.uploadPanel.showCancel(showCancel);
		this.getLayout().setActiveItem(0);
	},
	markHint: function(msg) {
		this.uploadPanel.setHint(msg);
		this.downloadPanel.setHint(msg);
	},
	markError: function(msg) {
		this.uploadPanel.setError(msg);
		this.downloadPanel.setError(msg);
	},
	clearHint: function() {
		this.uploadPanel.setHint('');
		this.downloadPanel.setHint('');
	},
	clearError: function() {
		this.uploadPanel.setError('');
		this.downloadPanel.setError('');
	}
});

Ext.ns('itasks.tui.document');

itasks.tui.document.DownloadPanel = Ext.extend(Ext.form.FormPanel,{
	
	initComponent: function(){
		this.dlButton = new Ext.Button(
			{ text : ''
			, qtip : 'Download'
			, iconCls : 'x-form-document-download-icon'
			, handler: this.dlButtonHandler
			});
		
		this.editButton = new Ext.Button(
			{ text : ''
			, qtip : 'Edit'
			, iconCls : 'x-form-document-edit-icon'
			, handler : this.editButtonHandler
			});
			
		this.trashButton = new Ext.Button(
			{ text : ''
			, qtip : 'Remove'
			, iconCls : 'x-form-document-trash-icon'
			, handler : this.trashButtonHandler
			});
			
		this.errorIcon = new Ext.Panel({cls: 'x-document-invalid-icon', width: 25, unstyled: true, hidden: true});
		this.hintIcon = new Ext.Panel({cls: 'x-document-hint-icon', width: 25, unstyled: true, hidden: true});
			
		Ext.apply(this,
		{ layout : 'hbox'
		, unstyled : true
		, fileUpload : true
		, height: 24
		, items : [
			{ width: 16
			, height: 16
			, xtype: 'panel'
			, unstyled: true
			},
			{ xtype: 'displayfield'
			, width: 262
			, height: 24
			, autoHeight: true
			, style: 'padding: 4px'
			},
			this.editButton,
			this.trashButton,
			this.dlButton,
			this.hintIcon,
			this.errorIcon
			]
		});
		
		itasks.tui.document.DownloadPanel.superclass.initComponent.apply(this,arguments);
	},
	
	setStaticDisplay : function(isStatic){
		this.editButton.setVisible(!isStatic);
		this.trashButton.setVisible(!isStatic);
		this.dlButton.setVisible(!isStatic);
	},
	
	updateDocInfo: function(docInfo){
		this.document = docInfo;
		
		var fn = this.document.name;
		var fs = this.printFileSize(this.document.size);
		
		this.get(1).setValue(fn+" ("+fs+")");
		
		this.get(0).cls = '';
		this.get(0).addClass(this.iconMimeType(this.document.mime));
	},
	
	iconMimeType: function(mime){
	
		if(mime.match("application/pdf")){
			return('file-type-application-pdf-icon');
		}
		else if(mime.match('^image')){
			return('file-type-image-icon');
		}
		else if(mime.match("^text")){
			return('file-type-text-icon');
		}
		else if(mime.match("^application")){
			return('file-type-application-icon');
		}
		else if(mime.match("^audio")){
			return('file-type-audio-icon');
		}
		else if(mime.match("^video")){
			return('file-type-video-icon');
		}
		else{
			return('file-type-plain-icon');
		}
	},
	
	printFileSize: function(size){
		if(size > 1048576){
			return (size/1048576).toFixed(1)+" Mbytes"
		}else if(size > 1024){
			return (size/1024).toFixed(1)+" Kbytes"
		}else{
			return size+" bytes"
		}
	},
	
	setError : function(msg){
		if(this.rendered){
			if(msg == '' || msg == null){
				this.errorIcon.hide();
			}else{
				this.hintIcon.hide();
				this.errorIcon.show();
				if(this.errorIcon.el) this.errorIcon.el.dom.qtip = msg;
				if(this.errorIcon.el) this.errorIcon.el.dom.qclass = 'x-form-invalid-tip'
			}
		}	
		
		this.doLayout();
	},
	
	setHint : function(msg){
		if(this.rendered){
			if(msg == '' || msg == null || this.errorIcon.isVisible()){
				this.hintIcon.hide();
			}else{
				this.hintIcon.show();
				if(this.hintIcon.el) this.hintIcon.el.dom.qtip = msg;
				if(this.hintIcon.el) this.hintIcon.el.dom.qclass = 'x-form-hint-tip'
			}
		}	
		
		this.doLayout();
	},
	
	dlButtonHandler: function(src, evt){
		var form = this.findParentByType('itasks.tui.document.Download');
		var dp = this.findParentByType('itasks.tui.Document');
		var wt = this.findParentByType('itasks.work');
		
		if(form.getForm().isValid()){
			form.getForm().submit({
			
				url: "/services/json/documents/" + dp.document.documentId + "/download",
				params: { session : itasks.app.session },
				waitMsg: null,
				failure: function(form, o){
					Ext.Msg.alert('Error',o.result.errors,function(){wt.refresh()});
				}
			});
		}
	},
	
	editButtonHandler: function(src, evt){
		this.findParentByType('itasks.tui.Document').showUploadPanel(true);
	},
	
	trashButtonHandler: function(src, evt){
	
		var tf = this.findParentByType(itasks.ttc.FormContainer);
		var dp = this.findParentByType('itasks.tui.Document');
	
		dp.fireEvent('tuichange',dp.taskId,dp.name,null);
	},
	
	setValue: function(value){
		this.ownerCt.setValue(value);
	}
});

itasks.tui.document.UploadPanel = Ext.extend(Ext.form.FormPanel,{

	initComponent: function(){
	
		this.cancelButton = new Ext.Button(
			{ text : ''
			, iconCls: 'x-form-document-cancel-icon'
			, handler: this.cancelButtonHandler			
			});
			
		this.errorIcon = new Ext.Panel({cls: 'x-document-invalid-icon', width: 25, unstyled: true, hidden: true});
		this.hintIcon = new Ext.Panel({cls: 'x-document-hint-icon', width: 25, unstyled: true, hidden: true});
			
		Ext.apply(this,
		{ fileUpload: true
		, unstyled: true
		, layout: 'hbox'
		, autoHeight: true
		, items: [
			{ xtype: 'fileuploadfield'
			, width: 300
			, id: 'form-file-'+this.id+'-'+this.name
			, emptyText: 'Select a document'
			, name: 'document'
			, buttonText: ''
			, buttonCfg: { iconCls: 'x-form-document-browse-icon' }
			, listeners: { fileselected : this.uplHandler }
			},
			this.cancelButton,
			this.hintIcon,
			this.errorIcon
			]
		});
		
		itasks.tui.document.UploadPanel.superclass.initComponent.apply(this,arguments);
	},
	
	setError : function(msg){
		if(this.rendered){
			if(msg == '' || msg == null){
				this.errorIcon.hide();
			}else{
				this.hintIcon.hide();
				this.errorIcon.show();
				if(this.errorIcon.el) this.errorIcon.el.dom.qtip = msg;
				if(this.errorIcon.el) this.errorIcon.el.dom.qclass = 'x-form-invalid-tip'
			}
		}	
		
		this.doLayout();
	},
	setHint: function(msg){
		if(this.rendered){
			if(msg == '' || msg == null || this.errorIcon.isVisible()){
				this.hintIcon.hide();
			}else{
				this.hintIcon.show();
				if(this.hintIcon.el) this.hintIcon.el.dom.qtip = msg;
				if(this.hintIcon.el) this.hintIcon.el.dom.qclass = 'x-form-hint-tip'
			}
		}	
		
		this.doLayout();
	},
	showCancel: function(cancel){
		this.cancelButton.setVisible(cancel);
	},
	uplHandler: function(src,evt){
	
		var form = this.findParentByType("itasks.tui.document.Upload");
		var dp = this.findParentByType("itasks.tui.Document");
						
		if(form.getForm().isValid()){				
			form.getForm().submit({
				url: "/services/json/documents/upload",
				
				params: { session : itasks.app.session },
				waitMsg: "Uploading document. Please wait..",
				success: function(form,response) {
						try {
							var resp = Ext.decode(response.response.responseText);
	
							dp.fireEvent('tuichange',dp.taskId,dp.name, resp.documents[0].documentId);
						} catch(e) {
							itasks.app.restart("Document transaction failed");
							return;
						}
					},
				failure: function(form,response) {
						itasks.app.restart("Document transaction failed");
						return;
					}					
			});
		}
	},
	cancelButtonHandler: function(src,evt){
		var form = this.findParentByType('itasks.tui.Document');
		form.showDownloadPanel(false);
	},
	
	setValue: function(value){
		this.ownerCt.setValue(value);
	}
});

Ext.reg("itasks.tui.Document",itasks.tui.DocumentControl);
Ext.reg("itasks.tui.document.Download",itasks.tui.document.DownloadPanel);
Ext.reg("itasks.tui.document.Upload",itasks.tui.document.UploadPanel);
