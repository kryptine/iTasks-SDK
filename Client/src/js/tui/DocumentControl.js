Ext.ns('itasks.tui');

itasks.tui.DocumentControl = Ext.extend(Ext.Panel,
{	
	initComponent: function(){
		
		Ext.apply(this,
		{ unstyled: true
		, layout: 'anchor'
		, height: 26
		, bodyStyle: 'margin: 0px 0px 2px 0px'
		, width: 500
		, allowUpload: true
		, defaults: {
			height: 26,
			bodyStyle: 'padding: 0px 2px 0px 0px;',
			anchor: '99%'
		}});
		
		itasks.tui.DocumentControl.superclass.initComponent.apply(this,arguments);	
		this.docInfo = Ext.decode(this.docInfo);
		this.addClass('x-form-document');
	},
	
	afterRender: function(arguments){
		itasks.tui.DocumentControl.superclass.afterRender.call(this,arguments);
		
		if(this.staticDisplay){
			if(this.docInfo.content != "EmptyDocument") this.showDownloadPanel();
			else {
				this.removeAll();
				this.add({html: 'Empty Document', unstyled: true});
			}
		}else{
			if(this.docInfo.content != "EmptyDocument") this.showDownloadPanel();
			else this.showUploadPanel(false);
		}
	},
	
	showUploadPanel: function(showCancel){
		this.removeAll();
		this.add(new itasks.tui.document.UploadPanel({name: this.name, showCancel: showCancel}));
	},
	
	showDownloadPanel: function(){
		this.removeAll();
		this.add(new itasks.tui.document.DownloadPanel({docInfo: this.docInfo, staticDisplay: this.staticDisplay}));
	}
});

Ext.ns('itasks.tui.document');

itasks.tui.document.DownloadPanel = Ext.extend(Ext.form.FormPanel,
{
	initComponent: function(){
	
		this.dlButton = new Ext.Button({
			text: 'Download',
			iconCls: 'x-form-document-download-icon',	
			renderTarget: 'td.x-form-document-download-button',
			handler: function(src,evt){
				var form = this.findParentByType('itasks.tui.document.download');
				var wt 	 = this.findParentByType('itasks.work');
				
				if(form.getForm().isValid()){
					form.getForm().submit({
						url: itasks.config.serverUrl+"/document/download",
						
						params: { _session : itasks.app.session
									, docInfo  : Ext.encode(form.docInfo)
									},
						
						waitMsg: null,
						
						failure: function(form, o){							
							Ext.Msg.alert('Error',o.result.errors,function(){wt.refresh()});
						}				
					});
				}
			}
		});
		
		this.editButton = new Ext.Button({
			text : '',
			iconCls: 'x-form-document-edit-icon',
			renderTarget: 'td.x-form-document-edit-button',
			handler: function(src,evt){
				var form = this.findParentByType('itasks.tui.Document');
				form.showUploadPanel(true);
				form.doLayout();
			}
		});
	
		this.trashButton = new Ext.Button({
			text : '',
			iconCls: 'x-form-document-trash-icon',
			renderTarget: 'td.x-form-document-trash-button',
			handler: function(src,evt){
				var form = this.findParentByType("itasks.tui.document.download");
				var tf = this.findParentByType(itasks.ttc.FormContainer);
				var wt = this.findParentByType("itasks.work");
				var dp = this.findParentByType("itasks.tui.Document");
				
				var params = { _session : itasks.app.session
					, _targettask : tf.taskId
					, _maintask : wt.taskId
					, _debug : itasks.app.debug ? 1 : 0
					, _name : dp.name
					, docInfo: Ext.encode(dp.docInfo)
				};
					
				form.getForm().submit({
					url: itasks.config.serverUrl+"/document/clear",
					params: params,
												
					success: function(form, response){							
						wt.refresh();														
					},

					failure: function(form, response){
						itasks.app.restart("Document transaction failed");
						return;
					}
				});
			}
		});
		
		Ext.apply(this,
		{ layout: 'ux.html'
		, unstyled: true
		, fileUpload: true
		, html:
			'<div style="position: relative; top: 0px;"><table><tr><td class="x-form-document-icon"></td><td class="x-form-document-fileinfo"></td><td class="x-form-document-edit-button"></td><td class="x-form-document-trash-button"></td><td class="x-form-document-download-button"></td></tr></table></div>'
		, items: [
			{ xtype: 'displayfield'
			, renderTarget: 'td.x-form-document-fileinfo'
			, value: 'Test'
			},
			{ xtype: 'panel'
			, renderTarget: 'td.x-form-document-icon'
			, width: 16
			, height: 16
			, unstyled: true
			},
			(this.staticDisplay)?{}:this.editButton,
			(this.staticDisplay)?{}:this.trashButton,
			this.dlButton
		]
		});
		
		itasks.tui.document.DownloadPanel.superclass.initComponent.apply(this,arguments);
		
		var filename = this.docInfo.content[1].fileName;
		var filesize = this.printFileSize(this.docInfo.content[1].size);
	
		this.items.items[0].value = filename+" ("+filesize+")"
		this.items.items[1].addClass(this.iconMimeType(this.docInfo.content[1].mimeType));
		
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
	}
});

itasks.tui.document.UploadPanel = Ext.extend(Ext.form.FormPanel,
{
	initComponent: function(){
	
		this.uplButton = new Ext.Button({
			text: 'Upload',
			iconCls: 'x-form-document-upload-icon',
			renderTarget: 'td.x-form-document-upload-button',
            handler: function(src,evt){ 
				
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
			}
		});
		
		this.cancelButton = new Ext.Button({
			text : '',
			iconCls : 'x-form-document-cancel-icon',
			renderTarget : 'td.x-form-document-cancel-button',
			handler: function(src,evt){
				var form = this.findParentByType('itasks.tui.Document');
				form.showDownloadPanel();
				form.doLayout();
			}
		});		
						
		Ext.apply(this,
		{ fileUpload: true
		, unstyled: true
		, layout: 'hbox'
		, defaults: {
			allowBlank: false,
			msgTarget: 'qtip',
			border: false,
			height: 24
		}
		, items : [
			{	xtype: 'fileuploadfield',
				width: 300,
				id: 'form-file-'+this.id+'-'+this.name,
				emptyText: 'Select a document',
				name: 'document',
				buttonText: '',
				buttonCfg: {
					iconCls: 'x-form-document-browse-icon'
				},
				renderTarget: 'td.x-form-document-uploadfield'
			},
			this.uplButton
		]	
		});
		
		itasks.tui.document.UploadPanel.superclass.initComponent.apply(this,arguments);	
	},
	
	afterRender: function(arguments){
	
		itasks.tui.document.UploadPanel.superclass.afterRender.call(this,arguments);
		if(this.showCancel){
			this.add(this.cancelButton);
		}	
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
