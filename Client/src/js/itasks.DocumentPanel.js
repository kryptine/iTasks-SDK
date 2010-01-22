Ext.ns("itasks");

itasks.DocumentPanel = Ext.extend(Ext.Panel,
{	
	initComponent: function(){
		
		Ext.apply(this,
		{ unstyled: true
		, layout: 'anchor'
		, height: 24
		, bodyStyle: 'margin: 0px 0px 2px -2px'
		, defaults: {
			height: 24,
			bodyStyle: 'padding: 0px 2px 0px 0px;',
			anchor: '99%'
		}
		});
		
		itasks.DocumentPanel.superclass.initComponent.apply(this,arguments);	
		this.docInfo = Ext.decode(this.docInfo);
		this.addClass('x-form-document');
	},
	
	afterRender: function(arguments){
		itasks.DocumentPanel.superclass.afterRender.call(this,arguments);
		
		if(this.docInfo.fileName.length > 0){
			this.showDownloadPanel();
		}else{
			this.showUploadPanel(false);
		}
	},
	
	showUploadPanel: function(showCancel){
		this.removeAll();
		this.add(new itasks.document.UploadPanel({name:this.name,showCancel: showCancel}));
		this.doLayout();
	},
	
	showDownloadPanel: function(){
		this.removeAll();
		this.add(new itasks.document.DownloadPanel({docInfo: this.docInfo}));
		this.doLayout();
	}
});

Ext.ns("itasks.document");

itasks.document.DownloadPanel = Ext.extend(Ext.form.FormPanel,
{
	initComponent: function(){
	
		this.dlButton = new Ext.Button({
			text: 'Download',
			iconCls: 'x-form-document-download-icon',	
			renderTarget: 'td.x-form-document-download-button',
			handler: function(src,evt){
				var form = this.findParentByType('itasks.document.download');
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
				var form = this.findParentByType('itasks.document');
				form.showUploadPanel(true);
			}
		});
	
		Ext.apply(this,
		{ layout: 'ux.html'
		, unstyled: true
		, fileUpload: true
		, html:
			'<div style="position: absolute; top: 0px;"><table><tr><td class="x-form-document-icon"></td><td class="x-form-document-fileinfo"></td><td class="x-form-document-edit-button"></td><td class="x-form-document-download-button"></td></tr></table></div>'
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
			this.editButton,
			this.dlButton
		]
		});
		
		itasks.document.DownloadPanel.superclass.initComponent.apply(this,arguments);
		
		var filename = this.docInfo.fileName;
		var filesize = this.printFileSize(this.docInfo.size);
	
		this.items.items[0].value = filename+" ("+filesize+")"
		this.items.items[1].addClass(this.iconMimeType(this.docInfo.mimeType));
		
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

itasks.document.UploadPanel = Ext.extend(Ext.form.FormPanel,
{
	initComponent: function(){
	
		this.uplButton = new Ext.Button({
			text: 'Upload',
			iconCls: 'x-form-document-upload-icon',
			renderTarget: 'td.x-form-document-upload-button',
            handler: function(src,evt){ 
				var form = this.findParentByType("itasks.document.upload");
				var tf = this.findParentByType("itasks.task-ext-form");
				var wt = this.findParentByType("itasks.work");
				var dp = this.findParentByType("itasks.document");
								
				if(form.getForm().isValid()){				
										
					form.getForm().submit({
												
						url: itasks.config.serverUrl+"/work/tab",
												
						params: { _session : itasks.app.session
								, _targettask : tf.taskId
								, _maintask : wt.taskId
								, _debug : itasks.app.debug ? 1 : 0
								, _name : dp.name
								, docInfo: Ext.encode(dp.docInfo)
								},
						
						waitMsg: 'Uploading document..',
						
						success: function(form, o){							
							try {
								data = Ext.decode(o.response.responseText);
							} catch(SyntaxError) {
								data = response.responseText;
							}
							
							if(typeof data == 'object'){
								wt.update(data);
								wt.refresh();
							}else{
								Ext.msg.alert('Error',data);
							}														
						},

						failure: function(form, o){
							try {
								data = Ext.decode(o.response.responseText);
							} catch(SyntaxError) {
								data = response.responseText;
							}
						
							itasks.app.restart(data.error);
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
				var form = this.findParentByType('itasks.document');
				form.showDownloadPanel();
			}
		})
				
		Ext.apply(this,
		{ fileUpload: true
		, unstyled: true
		, layout: 'ux.html'
		, defaults: {
			allowBlank: false,
			msgTarget: 'qtip',
			border: false,
			height: 24
		}
		, html: 	
			'<div style="position: absolute; top: 0px;"><table><tr><td class="x-form-document-uploadfield" style="width: 300px"></td><td class="x-form-document-cancel-button"></td><td class="x-form-document-upload-button"></td></tr></table></div>'
		, items : [{
			xtype: 'fileuploadfield',
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
		
		itasks.document.UploadPanel.superclass.initComponent.apply(this,arguments);	
	},
	
	afterRender: function(arguments){
	
		itasks.document.UploadPanel.superclass.afterRender.call(this,arguments);
		if(this.showCancel){
			this.add(this.cancelButton);
		}	
	}
});

Ext.reg("itasks.document",itasks.DocumentPanel);
Ext.reg("itasks.document.download",itasks.document.DownloadPanel);
Ext.reg("itasks.document.upload",itasks.document.UploadPanel);