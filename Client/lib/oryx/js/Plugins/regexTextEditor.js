
if (!ORYX.Plugins)
    ORYX.Plugins = new Object();

/**
 * Plugin that provides a Text Field Editor with Regex matching capabilities.
 * Values that don't match the provided regex are considered invalid and are not
 * set to the field.
 *
 * Example of a field using Regex Text Editor:
 *      {
            "id":"script_language",
            "type":"Regex",
            "title":"ScriptLanguage",
            "value":"",
            "description":"Defines the script language. The script language MUST be provided if a script is provided.",
            "readonly":false,
            "optional":true,
            "regex": /[a-zA-Z].+/,
            "invalidText": "The value must start with a letter and have at least 2 characters"
        }
 *
 * The defined type is "Regex", and the attributes used to configure this
 * editor are:
 *    1.- regex: A valid JavaScript regex.
 *    2.- invalidText: the text that is shown (as a tooltip) when the provided
 *                     value doesn' match the regex
 */
ORYX.Plugins.RegexTextEditor = Clazz.extend({

    construct: function(facade){
        this.facade = facade;
        ORYX.FieldEditors["regex"] = new ORYX.Plugins.RegexTextEditor.EditorFactory();
    }
});


ORYX.Plugins.RegexTextEditor.EditorFactory = Clazz.extend({
    construct: function(){

    },
    /**
     * This function gets executed by propertyWindow in its own context,
     * so this = propertyWindow
     */
    init: function(){
        //arguments: key, pair, icons, index
        var key = arguments[0];
        var pair = arguments[1];
        var editorTextArea = new Ext.form.TextArea({
            alignment: "tl-tl",
            allowBlank: pair.optional(),
            msgTarget:'title',
            maxLength:pair.length(),
            regex: pair._jsonProp.regex,
            regexText: pair._jsonProp.invalidText
        });

        editorTextArea.on('keyup', function(textArea, event) {
            if (editorTextArea.validate()){
                this.editDirectly(key, textArea.getValue());
            }
        }.bind(this));

        return new Ext.Editor(editorTextArea);
    }
});
