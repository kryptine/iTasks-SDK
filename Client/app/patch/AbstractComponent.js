/**
* Override of some methods of AbstractComponent because of special treatment of
* the values 'wrap' and 'flex' in the size properties.
*/
Ext.define('itwc.patch.AbstractComponent', {
	override: 'Ext.AbstractComponent',

    /**
     * Applies padding, margin, border, top, left, height, and width configs to the
     * appropriate elements.
     * @private
     */
    initStyles: function(targetEl) {
        var me = this,
            Element = Ext.Element,
            margin = me.margin,
            border = me.border,
            cls = me.cls,
            style = me.style,
            x = me.x,
            y = me.y,
            width, height;

        me.initPadding(targetEl);

        if (margin != null) {
            targetEl.setStyle('margin', this.unitizeBox((margin === true) ? 5 : margin));
        }

        if (border != null) {
            me.setBorder(border, targetEl);
        }

        // initComponent, beforeRender, or event handlers may have set the style or cls property since the protoEl was set up
        // so we must apply styles and classes here too.
        if (cls && cls != me.initialCls) {
            targetEl.addCls(cls);
            me.cls = me.initialCls = null;
        }
        if (style && style != me.initialStyle) {
            targetEl.setStyle(style);
            me.style = me.initialStyle = null;
        }

        if (x != null) {
            targetEl.setStyle(me.horizontalPosProp, (typeof x == 'number') ? (x + 'px') : x);
        }
        if (y != null) {
            targetEl.setStyle('top', (typeof y == 'number') ? (y + 'px') : y);
        }

        // Framed components need their width/height to apply to the frame, which is
        // best handled in layout at present.
        if (!me.getFrameInfo()) {
            width = me.width;
            height = me.height;

            // If we're using the content box model, we also cannot assign numeric initial sizes since we do not know the border widths to subtract
            if (width != null) {
                if (typeof width === 'number') {
                    if (Ext.isBorderBox) {
                        targetEl.setStyle('width', width + 'px');
                    }
                } else if (width !== 'flex' && width !== 'wrap') { //OVERRIDE
                    targetEl.setStyle('width', width);
                }
            }
            if (height != null) {
                if (typeof height === 'number') {
                    if (Ext.isBorderBox) {
                        targetEl.setStyle('height', height + 'px');
                    }
                } else if (height !== 'flex' && height !== 'wrap') { //OVERRIDE
                    targetEl.setStyle('height', height);
                }
            }
        }
    },
    /**
     * Returns an object that describes how this component's width and height are managed.
     * All of these objects are shared and should not be modified.
     *
     * @return {Object} The size model for this component.
     * @return {Ext.layout.SizeModel} return.width The {@link Ext.layout.SizeModel size model}
     * for the width.
     * @return {Ext.layout.SizeModel} return.height The {@link Ext.layout.SizeModel size model}
     * for the height.
     */
    getSizeModel: function (ownerCtSizeModel) {
        var me = this,
            models = Ext.layout.SizeModel,
            ownerContext = me.componentLayout.ownerContext,
            width = me.width,
            height = me.height,
            typeofWidth, typeofHeight,
            hasPixelWidth, hasPixelHeight,
            heightModel, ownerLayout, policy, shrinkWrap, topLevel, widthModel;

        if (ownerContext) {
            // If we are in the middle of a running layout, always report the current,
            // dynamic size model rather than recompute it. This is not (only) a time
            // saving thing, but a correctness thing since we cannot get the right answer
            // otherwise.
            widthModel = ownerContext.widthModel;
            heightModel = ownerContext.heightModel;
        }

        if (!widthModel || !heightModel) {
            hasPixelWidth = ((typeofWidth = typeof width) == 'number');
            hasPixelHeight = ((typeofHeight = typeof height) == 'number');
            topLevel = me.floating || !(ownerLayout = me.ownerLayout);

            // Floating or no owner layout, e.g. rendered using renderTo
            if (topLevel) {
                policy = Ext.layout.Layout.prototype.autoSizePolicy;
                shrinkWrap = me.floating ? 3 : me.shrinkWrap;

                if (hasPixelWidth) {
                    widthModel = models.configured;
                }

                if (hasPixelHeight) {
                    heightModel = models.configured;
                }
            } else {
                policy = ownerLayout.getItemSizePolicy(me, ownerCtSizeModel);
                shrinkWrap = ownerLayout.isItemShrinkWrap(me);
            }

            if (ownerContext) {
                ownerContext.ownerSizePolicy = policy;
            }

            shrinkWrap = (shrinkWrap === true) ? 3 : (shrinkWrap || 0); // false->0, true->3

            // Now that we have shrinkWrap as a 0-3 value, we need to turn off shrinkWrap
            // bits for any dimension that has a configured size not in pixels. These must
            // be read from the DOM.
            //
            if (topLevel && shrinkWrap) {
                if (width && typeofWidth == 'string' && width !== 'wrap') { //OVERRIDE
                    shrinkWrap &= 2; // percentage, "30em" or whatever - not width shrinkWrap
                }
                if (height && typeofHeight == 'string' && height !== 'wrap') { //OVERRIDE
                    shrinkWrap &= 1; // percentage, "30em" or whatever - not height shrinkWrap
                }
            }

            if (shrinkWrap !== 3) {
                if (!ownerCtSizeModel) {
                    ownerCtSizeModel = me.ownerCt && me.ownerCt.getSizeModel();
                }

                if (ownerCtSizeModel) {
                    shrinkWrap |= (ownerCtSizeModel.width.shrinkWrap ? 1 : 0) | (ownerCtSizeModel.height.shrinkWrap ? 2 : 0);
                }
            }

            if (!widthModel) {
                if (!policy.setsWidth) {
                    if (hasPixelWidth) {
                        widthModel = models.configured;
                    } else {
                        widthModel = (shrinkWrap & 1) ? models.shrinkWrap : models.natural;
                    }
                } else if (policy.readsWidth) {
                    if (hasPixelWidth) {
                        widthModel = models.calculatedFromConfigured;
                    } else {
                        widthModel = (shrinkWrap & 1) ? models.calculatedFromShrinkWrap :
                                    models.calculatedFromNatural;
                    }
                } else {
                    widthModel = models.calculated;
                }
            }

            if (!heightModel) {
                if (!policy.setsHeight) {
                    if (hasPixelHeight) {
                        heightModel = models.configured;
                    } else {
                        heightModel = (shrinkWrap & 2) ? models.shrinkWrap : models.natural;
                    }
                } else if (policy.readsHeight) {
                    if (hasPixelHeight) {
                        heightModel = models.calculatedFromConfigured;
                    } else {
                        heightModel = (shrinkWrap & 2) ? models.calculatedFromShrinkWrap :
                                    models.calculatedFromNatural;
                    }
                } else {
                    heightModel = models.calculated;
                }
            }
        }
        // We return one of the cached objects with the proper "width" and "height" as the
        // sizeModels we have determined.
        return widthModel.pairsByHeightOrdinal[heightModel.ordinal];
    }
});
