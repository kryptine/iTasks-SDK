Ext.ns('itasks.tui');

itasks.tui.DocumentControl = Ext.extend(Ext.Panel,
{	
	initComponent : function(){
		this.uploadPanel = new itasks.tui.document.UploadPanel({name: this.name, id: this.id});
		this.downloadPanel = new itasks.tui.document.DownloadPanel();
		
		Ext.apply(this,
		{ unstyled: true
		, width: 500
		, layout: 'card'
		, activeItem: 0
		, autoHeight: true
		, items: [
				this.uploadPanel,
				this.downloadPanel
			]
		});
	
		this.docInfo = Ext.decode(this.docInfo);
		itasks.tui.DocumentControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender : function(arguments){
		itasks.tui.DocumentControl.superclass.afterRender.call(this,arguments);
		
		if(this.staticDisplay){
			if(this.docInfo.content != "EmptyDocument"){
				this.showDownloadPanel(true);
			}else{
				this.removeAll();
				this.add({html: 'Empty Document', unstyled: true});
			}				
		}else if(this.docInfo.content != "EmptyDocument"){
			this.showDownloadPanel(false);
		}else{
			this.showUploadPanel(false);
		}
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
		
		this.doLayout();
	},
	
	showDownloadPanel : function(isStatic){
		this.downloadPanel.updateDocInfo(this.docInfo);
		this.downloadPanel.setStaticDisplay(isStatic);
		this.getLayout().setActiveItem(1);
	},
	
	showUploadPanel : function(showCancel){
		this.uploadPanel.showCancel(showCancel);
		this.getLayout().setActiveItem(0);
	},
	
	setError: function(msg){
		if(this.staticDisplay) return;
		
		(function(){
			this.uploadPanel.setError(msg);
			this.downloadPanel.setError(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		if(this.staticDisplay) return;
		
		(function(){
			this.uploadPanel.setHint(msg);
			this.downloadPanel.setHint(msg);
		}).defer(50,this);
	}
});

Ext.ns('itasks.tui.document');

itasks.tui.document.DownloadPanel = Ext.extend(Ext.form.FormPanel,{
	
	initComponent: function(){
		this.dlButton = new Ext.Button(
			{ text : 'Download'
			, iconCls : 'x-form-document-download-icon'
			, handler: this.dlButtonHandler
			});
		
		this.editButton = new Ext.Button(
			{ text : ''
			, iconCls : 'x-form-document-edit-icon'
			, handler : this.editButtonHandler
			});
			
		this.trashButton = new Ext.Button(
			{ text : ''
			, iconCls : 'x-form-document-trash-icon'
			, handler : this.trashButtonHandler
			});
			
		this.errorIcon = new Ext.Panel({cls: 'x-document-invalid-icon', width: 25, hideMode: 'display', unstyled: true, hidden: true});
		this.hintIcon = new Ext.Panel({cls: 'x-document-hint-icon', width: 25, hideMode: 'display', unstyled: true, hidden: true});
			
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
			this.errorIcon,
			this.hintIcon
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
		this.docInfo = docInfo;
		
		var fn = this.docInfo.content[1].fileName;
		var fs = this.printFileSize(this.docInfo.content[1].size);
		
		this.get(1).setValue(fn+" ("+fs+")");
		
		this.get(0).cls = '';
		this.get(0).addClass(this.iconMimeType(this.docInfo.content[1].mimeType));
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
			if(msg == '' || msg == null){
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
		var form = this.findParentByType('itasks.tui.document.download');
		var wt = this.findParentByType('itasks.work');
		
		if(form.getForm().isValid()){
			form.getForm().submit({
				url: itasks.config.serverUrl+'/document/download',
				params: { _session : itasks.app.session, docInfo: Ext.encode(form.docInfo) },
				waitMsg: null,
				failure: function(form, o){
					Ext.Msg.alert('Error',o.result.errors,function(){wt.refresh()});
				}
			});
		}
	},
	
	editButtonHandler: function(src, evt){
		var form = this.findParentByType('itasks.tui.Document');
		form.showUploadPanel(true);
	},
	
	trashButtonHandler: function(src, evt){
		var form = this.findParentByType('itasks.tui.document.download');
		var tf = this.findParentByType(itasks.ttc.FormContainer);
		var wt = this.findParentByType('itasks.work');
		var dp = this.findParentByType('itasks.tui.Document');
		
		var params =
			{ _session : itasks.app.session
			, _targettask : tf.taskId
			, _maintask : wt.taskId
			, _debug : itasks.app.debug ? 1 : 0
			, _name : dp.name
			, docInfo : Ext.encode(dp.docInfo)
			};
			
		form.getForm().submit({
			url: itasks.config.serverUrl+"/document/clear",
			params: params,
			success: function() { wt.refresh(); },
			failure: function() { itasks.app.restart("Document transaction failed"); return; }
		});
	}
});

itasks.tui.document.UploadPanel = Ext.extend(Ext.form.FormPanel,{

	initComponent: function(){
		this.uplButton = new Ext.Button(
			{ text : 'Upload'
			, iconCls: 'x-form-document-upload-icon'
			, handler: this.uplButtonHandler
			});
		
		this.cancelButton = new Ext.Button(
			{ text : ''
			, iconCls: 'x-form-document-cancel-icon'
			, handler: this.cancelButtonHandler			
			});
			
		this.errorIcon = new Ext.Panel({cls: 'x-document-invalid-icon', width: 25, hideMode: 'display', unstyled: true, hidden: true});
		this.hintIcon = new Ext.Panel({cls: 'x-document-hint-icon', width: 25, hideMode: 'display', unstyled: true, hidden: true});
			
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
			},
			this.cancelButton,
			this.uplButton,
			this.errorIcon,
			this.hintIcon
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
	
	setHint : function(msg){
		if(this.rendered){
			if(msg == '' || msg == null){
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
	
	uplButtonHandler : function(src,evt){
		var form = this.findParentByType("itasks.tui.document.upload");
		var tf = this.findParentByType(itasks.ttc.FormContainer);
		var wt = this.findParentByType("itasks.work");
		var dp = this.findParentByType("itasks.tui.Document");
						
		if(form.getForm().isValid()){				
			form.getForm().submit({
				url: itasks.config.serverUrl+"/document/upload",
				
				params: 
					{ _session : itasks.app.session
					, _targettask : tf.taskId
					, _maintask : wt.taskId
					, _debug : itasks.app.debug ? 1 : 0
					, _name : dp.name
					, docInfo: Ext.encode(dp.docInfo)
					},
				waitMsg: 'Uploading document. Please wait..',
				success: function(form,response)
					{
						wt.refresh();
					},
				failure: function(form,response)
					{
						itasks.app.restart("Document transaction failed");
						return;
					}					
			});
		}
	},
	
	cancelButtonHandler: function(src,evt){
		var form = this.findParentByType('itasks.tui.Document');
		form.showDownloadPanel(false);
	}
});

Ext.reg("itasks.tui.Document",itasks.tui.DocumentControl);
Ext.reg("itasks.tui.document.download",itasks.tui.document.DownloadPanel);
Ext.reg("itasks.tui.document.upload",itasks.tui.document.UploadPanel);

//--- Preview function ---
itasks.preview = function(link){
	
	var vport = Ext.getDoc().getViewSize();
		
	var prevWindow = new Ext.Window(
		{ modal: true
		, width: vport.width*.9
		, height: vport.height*.9
		, title: 'Document Preview'
		, layout: 'fit'
		, items: [
			{ xtype: 'iframepanel'
			, frameCfg: { name: 'preview_frame' } 
			, defaultSrc: link
			, header: false
			}
		]
		}
	);
		
	prevWindow.show();
};
