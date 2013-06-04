/**
 * Copyright (c) 2008, Gero Decker, refactored by Kai Schlichting
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
if (!ORYX.Plugins)
    ORYX.Plugins = new Object();

/**
   This plugin is a generic syntax checker for different diagram types.
   Needs server communication.
   @class ORYX.Plugins.SyntaxChecker
   @constructor Creates a new instance
   @extends ORYX.Plugins.AbstractPlugin
*/
ORYX.Plugins.SyntaxChecker = ORYX.Plugins.AbstractPlugin.extend({
    /**@private*/
    construct: function(){
        arguments.callee.$.construct.apply(this, arguments);

        this.raisedEventIds = [];

        this.facade.registerOnEvent(ORYX.Plugins.SyntaxChecker.RESET_ERRORS_EVENT, this.resetErrors.bind(this));
        this.facade.registerOnEvent(ORYX.Plugins.SyntaxChecker.SHOW_ERRORS_EVENT, this.doShowErrors.bind(this));
    },

    /** Called on SHOW_ERRORS_EVENT.
     *
     * @param {Object} event
     * @param {Object} args
     */
    doShowErrors: function(event, args){
        this.showErrors(event.errors);
    },

    /**
     * Shows overlays for each given error
     * @methodOf ORYX.Plugins.SyntaxChecker.prototype
     * @param {Hash|Object} errors
     * @example
     * showErrors({
     *     myShape1: "This has an error!",
     *     myShape2: "Another error!"
     * })
     */
    showErrors: function(errors){
        // If normal object is given, convert to hash
        if(!(errors instanceof Hash)){
            errors = new Hash(errors);
        }

        // Get all Valid ResourceIDs and collect all shapes
        errors.keys().each(function(value){
            var sh = this.facade.getCanvas().getChildShapeByResourceId(value);
            if (sh) {
                this.raiseOverlay(sh, this.parseCodeToMsg(errors[value]));
            }
        }.bind(this));
    },
    parseCodeToMsg: function(code){
    	var msg = code.replace(/: /g, "<br />").replace(/, /g, "<br />");
    	var codes = msg.split("<br />");
    	for (var i=0; i < codes.length; i++) {
    		var singleCode = codes[i];
    		var replacement = this.parseSingleCodeToMsg(singleCode);
    		if (singleCode != replacement) {
    			msg = msg.replace(singleCode, replacement);
    		}
    	}

		return msg;
	},

	parseSingleCodeToMsg: function(code){
		return ORYX.I18N.SyntaxChecker[code]||code;
	},
    /**
     * Resets all (displayed) errors
     * @methodOf ORYX.Plugins.SyntaxChecker.prototype
     */
    resetErrors: function(){
        this.raisedEventIds.each(function(id){
            this.facade.raiseEvent({
                type: ORYX.CONFIG.EVENT_OVERLAY_HIDE,
                id: id
            });
        }.bind(this))

        this.raisedEventIds = [];
    },

    raiseOverlay: function(shape, errorMsg){
        var id = "syntaxchecker." + this.raisedEventIds.length;
        var crossId = ORYX.Editor.provideId();
        var cross = ORYX.Editor.graft("http://www.w3.org/2000/svg", null, ['path', {
            "id":crossId,
            "title":"",
            "stroke-width": 5.0,
            "stroke": "red",
            "d": "M20,-5 L5,-20 M5,-5 L20,-20",
            "line-captions": "round"
        }]);

        this.facade.raiseEvent({
            type: ORYX.CONFIG.EVENT_OVERLAY_SHOW,
            id: id,
            shapes: [shape],
            node: cross,
            nodePosition: shape instanceof ORYX.Core.Edge ? "START" : "NE",
		    forceExecution: true //Ext 3: cross node needs to be present before tooltip is created
        });

        var tooltip = new Ext.ToolTip({
        	showDelay:50,
        	html:errorMsg,
        	target:crossId
        });

        this.raisedEventIds.push(id);

        return cross;
    }
});

ORYX.Plugins.SyntaxChecker.RESET_ERRORS_EVENT = "resetErrors";
ORYX.Plugins.SyntaxChecker.SHOW_ERRORS_EVENT = "showErrors";
