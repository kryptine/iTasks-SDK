/**
 * Copyright (c) 2009
 * Jan-Felix Schwarz
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 **/


if (!ORYX.Plugins) {
	ORYX.Plugins = new Object();
}

ORYX.Plugins.SelectStencilSetPerspective = {

	facade: undefined,

	extensions : undefined,

	perspectives: undefined,

	construct: function(facade) {
		this.facade = facade;

		var url = ORYX.BASE_FILE_PATH + "stencilsets/extensions/extensions.json";

		new Ajax.Request(url, {
			method: 'GET',
			asynchronous: false,
			onSuccess: (function(transport) {
				try {
					var responseStr = transport.responseText;
					var jsonObject = responseStr.evalJSON();
					/* Determine available extensions */
					this.extensions = {};
					jsonObject.extensions.each(function(ext) {
						this.extensions[ext.namespace] = ext;
					}.bind(this));

					this.perspectives = {};
					jsonObject.perspectives.each(function(per) {
						this.perspectives[per.namespace] = per;
					}.bind(this));

					this.facade.getStencilSets().values().each((function(sset) {

						var validPerspectives = jsonObject.perspectives.findAll(function(perspective){
							if(perspective.stencilset == sset.namespace()) return true;
							else return false;
						});

						if(validPerspectives.size()>0)
							this.createPerspectivesCombobox(sset, validPerspectives);

					}).bind(this));

				} catch (e) {
					ORYX.Log.debug(ORYX.I18N.SSExtensionLoader.failed1);
					Ext.Msg.alert("Oryx", ORYX.I18N.SSExtensionLoader.failed1);
				}
			}).bind(this),
			onFailure: (function(transport){
				Ext.Msg.alert("Oryx", ORYX.I18N.SSExtensionLoader.failed2);
			}).bind(this)
		});

		var nsCookieVal = this._readCookie("designerperspective");
		if(nsCookieVal != null) {
			this._updateStencil(nsCookieVal);
		}
	},

	createPerspectivesCombobox: function(stencilset, perspectives) {
		var data = new Array();
		perspectives.each(function(perspective) {
			data.push([perspective.namespace, perspective.title, perspective.description]);
		});

		var store = new Ext.data.SimpleStore({
			fields: ['namespace', 'title', 'tooltip'],
			data: data
		});

		var combobox = new Ext.form.ComboBox({
			store: store,
			displayField:'title',
			forceSelection: true,
			typeAhead: true,
			mode: 'local',
			width: 168,
			triggerAction: 'all',
			emptyText:'Select a perspective...',
			selectOnFocus:true
		});

		combobox.on('select', this.onSelect ,this);


		var panel = new Ext.Panel({
				bodyStyle:'background:#eee;font-size:9px;font-family:Verdana, Geneva, Arial, Helvetica, sans-serif;',
				autoScroll:true,
				lines: false,
				items: [combobox],
				title: 'Change Perspective'
			});
			this.facade.addToRegion("west", panel);
		panel.show();
		panel.doLayout();
		//this.facade.addToRegion("west", panel);
	},

	onSelect: function(combobox, record) {
		var ns = record.json[0];
		this._updateStencil(ns);
		this._createCookie("designerperspective", ns, 365);
	},
	_updateStencil: function(ns) {
		/* Get loaded stencil set extensions */
		var stencilSets = this.facade.getStencilSets();
		var loadedExtensions = new Object();

		stencilSets.values().each(function(ss) {
    		ss.changeTitle(this.perspectives[ns].title);
	    	ss.extensions().values().each(function(extension) {
				if(this.extensions[extension.namespace])
					loadedExtensions[extension.namespace] = extension;
			}.bind(this));
		}.bind(this));

		/* Determine extensions that are required for this perspective */
		var addExtensions = new Array();

		if(this.perspectives[ns].addExtensions) {
			this.perspectives[ns].addExtensions.each(function(ext){
				if(!ext.ifIsLoaded) {
					addExtensions.push(this.extensions[ext]);
					return;
				}

				if(loadedExtensions[ext.ifIsLoaded] && this.extensions[ext.add]) {
					addExtensions.push(this.extensions[ext.add]);
				} else {
					if(ext["default"] && this.extensions[ext["default"]]) {
						addExtensions.push(this.extensions[ext["default"]]);
					}
				}
			}.bind(this));
		}

		/* Determine extension that are not allowed in this perspective */

		/* Check if flag to remove all other extension is set */
		if(this.perspectives[ns].removeAllExtensions) {
			this._loadExtensions(addExtensions, undefined, true);
			return;
		}

		/* Check on specific extensions */
		var removeExtensions = new Array();
		if(this.perspectives[ns].removeExtensions) {
			this.perspectives[ns].removeExtensions.each(function(ns){
				removeExtensions.push(this.extensions[ns]);
			}.bind(this));
		}
		this._loadExtensions(addExtensions, removeExtensions, false);
	},
	/*
	 * Load all stencil set extensions specified in param extensions (key map: String -> Object)
	 * Unload all other extensions (method copied from addssextension plugin)
	 */
	_loadExtensions: function(addExtensions, removeExtensions, removeAll) {
		var stencilsets = this.facade.getStencilSets();

		var atLeastOne = false;

		// unload unselected extensions
		stencilsets.values().each(function(stencilset) {
			var unselected = stencilset.extensions().values().select(function(ext) { return addExtensions[ext.namespace] == undefined });
			if(removeAll) {
				unselected.each(function(ext) {
					stencilset.removeExtension(ext.namespace);
					atLeastOne = true;
				});
			} else {
				unselected.each(function(ext) {
					var remove = removeExtensions.find(function(remExt) {
						return ext.namespace === remExt.namespace;
					});

					if(remove) {
						stencilset.removeExtension(ext.namespace);
						atLeastOne = true;
					}
				});
			}
		});

		// load selected extensions
		addExtensions.each(function(extension) {
			var stencilset = stencilsets[extension["extends"]];
			if(stencilset) {
				stencilset.addExtensionFromDefinition( ORYX.CONFIG.ROOT_PATH + "/stencilset/extensions/" + extension.definition);
				atLeastOne = true;
			}
		}.bind(this));

		if (atLeastOne) {
			stencilsets.values().each(function(stencilset) {
				this.facade.getRules().initializeRules(stencilset);
			}.bind(this));
			this.facade.raiseEvent({
				type: ORYX.CONFIG.EVENT_STENCIL_SET_LOADED
			});
			var selection = this.facade.getSelection();
			this.facade.setSelection();
			this.facade.setSelection(selection);
		}
	},
	_createCookie: function(name, value, days) {
		if (days) {
			var date = new Date();
			date.setTime(date.getTime()+(days*24*60*60*1000));
			var expires = "; expires="+date.toGMTString();
		}
		else {
			var expires = "";
		}

		document.cookie = name+"="+value+expires+"; path=/";
	},
	_readCookie: function(name) {
		var nameEQ = name + "=";
		var ca = document.cookie.split(';');
		for(var i=0;i < ca.length;i++) {
			var c = ca[i];
			while (c.charAt(0)==' ') c = c.substring(1,c.length);
			if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
		}
		return null;
	}

}



ORYX.Plugins.SelectStencilSetPerspective = Clazz.extend(ORYX.Plugins.SelectStencilSetPerspective);

