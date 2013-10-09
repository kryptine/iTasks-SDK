/*
 * iTasks uses size properties that are slightly different from
 * those used by ExtJS. For example height: 'flex'. or width:'wrap'.
 *
 * To prevent misinterpretation of these property values by ExtJS defined
 * component and container layouts, the properties are set serverside
 * with the 'itwc' prefix. E.g. itwcHeight and itwcMaxWidth.
 *
 * This mixin provides a function to set the sizing properties based on
 * these prefixed iTasks size properties.
 * It should be called in the initComponent function of all itwc components.
 */
Ext.define('itwc.Sizeable',{
    initSize: function() {

        //First version: dumb copy
        this.width = this.itwcWidth;
        this.minWidth = this.itwcMinWidth;
        this.maxWidth = this.itwcMaxWidth;

        this.height = this.itwcHeight;
        this.minHeight = this.itwcMinHeight;
        this.maxHeight = this.itwcMaxHeight;
    }
});
