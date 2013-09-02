/**
 * Customized all-in-one Box layout
 */

Ext.define('itwc.layout.container.Box', {

    /* Begin Definitions */

	alias: ['layout.itwc_box'],
	extend: 'Ext.layout.container.Container',

    requires: [
        'Ext.layout.container.boxOverflow.None',
        'Ext.layout.container.boxOverflow.Menu',
        'Ext.layout.container.boxOverflow.Scroller',
        'Ext.util.Format',
        'Ext.dd.DragDropManager'
    ],

    /* End Definitions */

    /**
     * @cfg {Object} defaultMargins
     * If the individual contained items do not have a margins property specified or margin specified via CSS, the
     * default margins from this property will be applied to each item.
     *
     * This property may be specified as an object containing margins to apply in the format:
     *
     *     {
     *         top: (top margin),
     *         right: (right margin),
     *         bottom: (bottom margin),
     *         left: (left margin)
     *     }
     *
     * This property may also be specified as a string containing space-separated, numeric margin values. The order of
     * the sides associated with each value matches the way CSS processes margin values:
     *
     *   - If there is only one value, it applies to all sides.
     *   - If there are two values, the top and bottom borders are set to the first value and the right and left are
     *     set to the second.
     *   - If there are three values, the top is set to the first value, the left and right are set to the second,
     *     and the bottom is set to the third.
     *   - If there are four values, they apply to the top, right, bottom, and left, respectively.
     */
    defaultMargins: {
        top: 0,
        right: 0,
        bottom: 0,
        left: 0
    },

    /**
     * @cfg {String} padding
     * Sets the padding to be applied to all child items managed by this layout.
     *
     * This property must be specified as a string containing space-separated, numeric padding values. The order of the
     * sides associated with each value matches the way CSS processes padding values:
     *
     *   - If there is only one value, it applies to all sides.
     *   - If there are two values, the top and bottom borders are set to the first value and the right and left are
     *     set to the second.
     *   - If there are three values, the top is set to the first value, the left and right are set to the second,
     *     and the bottom is set to the third.
     *   - If there are four values, they apply to the top, right, bottom, and left, respectively.
     */
    padding: 0,

	/**
     * @cfg {String} halign
     * Controls how the child items of the container are aligned vertically. 
     * Valid values of this property are:
     *
     *   - **left** - child items are aligned to the **left** of the container (**default**)
     *   - **center** - child items are aligned to the **center** of the container
     *   - **right** - child items are aligned to the **right** of the container
     */
	halign: 'left',

    /**
     * @cfg {String} valign
     * Controls how the child items of the container are aligned vertically. 
     * Valid values of this property are:
     *
     *   - **top** - child items are aligned to the **middle** of the container (**default**)
     *   - **middle** - child items are aligned to the **middle** of the container
     *   - **bottom** - child items are aligned to the **bottom** of the container
     */
    valign: 'top',

    /**
     * @cfg {String} direction 
	 * Controls the direction in which child items are layed out
	 * Valid values of this property are:
	 *   - **vertical** - child items are layed out vertically from top to bottom (**default**)
	 *   - **horizontal** - child items are layed out horizontally from left to right (**default**)
	 */

    direction: 'vertical',

    type: 'box',
    itemCls: Ext.baseCSSPrefix + 'box-item',
    targetCls: Ext.baseCSSPrefix + 'box-layout-ct',
	targetElCls: Ext.baseCSSPrefix + 'box-target',
    innerCls: Ext.baseCSSPrefix + 'box-inner',

    availableSpaceOffset: 0,

	reserveOffset: true,
    manageMargins: true,
	createsInnerCt: true,

    horizontal: false,

    childEls: [
        'innerCt',
        'targetEl'
    ],

	
	renderTpl: [
		'{%var oc,l=values.$comp.layout,oh=l.overflowHandler;',
		'if (oh.getPrefixConfig!==Ext.emptyFn) {',
			'if(oc=oh.getPrefixConfig())dh.generateMarkup(oc, out)',
		'}%}',
		'<div id="{ownerId}-innerCt" class="{[l.innerCls]} {[oh.getOverflowCls()]}" role="presentation">',
			'<div id="{ownerId}-targetEl" class="{targetElCls}">',
				'{%this.renderBody(out, values)%}',
			'</div>',
		'</div>',
		'{%if (oh.getSuffixConfig!==Ext.emptyFn) {',
			'if(oc=oh.getSuffixConfig())dh.generateMarkup(oc, out)',
		'}%}',
		{
			disableFormats: true,
			definitions: 'var dh=Ext.DomHelper;'
		}
	],

    constructor: function(config) {
        var me = this,
            type;

        me.callParent(arguments);

		//Set direction as boolean flag
		me.horizontal = me.direction == 'horizontal';

        me.initOverflowHandler();

        type = typeof me.padding;
        if (type == 'string' || type == 'number') {
            me.padding = Ext.util.Format.parseBox(me.padding);
            me.padding.height = me.padding.top  + me.padding.bottom;
            me.padding.width  = me.padding.left + me.padding.right;
        }

    },

    getItemSizePolicy: function (item, ownerSizeModel) {
		return {
			setsWidth: item.width === 'flex',
			setsHeight: item.height === 'flex',
			readsWidth: item.minWidth === 'wrap',
			readsHeight: item.minHeight === 'wrap'
		};
    },
    isItemBoxParent: function (itemContext) {
        return false; //return true;
    },
    isItemShrinkWrap: function (item) {
		return true;
    },


    beginLayout: function (ownerContext) {
        var me = this,
            style = me.innerCt.dom.style,
			valign = me.valign,
			halign = me.halign;

        // this must happen before callParent to allow the overflow handler to do its work
        // that can effect the childItems collection...
        me.overflowHandler.beginLayout(ownerContext);

        me.callParent(arguments);

        ownerContext.innerCtContext = ownerContext.getEl('innerCt', me);

        // Don't allow sizes burned on to the innerCt to influence measurements.
        style.width = '';
        style.height = '';

		// Cache align string compares
		ownerContext.valign = { //Default is 'top'
			middle: valign == 'middle',
			bottom: valign == 'bottom'
		};
		ownerContext.halign = { //Default is 'left'
			center: halign == 'center',
			right: halign == 'right'
		};

        me.cacheFlexes(ownerContext);
    },

    beginLayoutCycle: function (ownerContext, firstCycle) {
        var me = this,
            childItems, childItemsLength, childContext, i, shrinkWrap, calculated;

        // this must happen before callParent to allow the overflow handler to do its work
        // that can effect the childItems collection...
        me.overflowHandler.beginLayoutCycle(ownerContext, firstCycle);

        me.callParent(arguments);

		// If a dimension is set to shrink wrap we have to figure out how big child items that
		// have that dimension set to flex should be. If they have a minSize configured,
		// we can use that value. If the minSize is set to 'wrap' then we need to switch the
		// sizeModel of those child items to shrinkWrap

		childItems = ownerContext.childItems;
		childItemsLength = childItems.length;
		shrinkWrap = me.sizeModels.shrinkWrap;

		for (i = 0; i < childItemsLength; i++) {
			childContext = childItems[i];
			if(childContext.target.width === 'wrap' || (childContext.target.width === 'flex' && childContext.target.minWidth === 'wrap')) {
				childContext.widthModel = shrinkWrap;
			}
			if(childContext.target.height === 'wrap' || (childContext.target.height === 'flex' && childContext.target.minHeight === 'wrap')) {
				childContext.heightModel = shrinkWrap;
			}
		}

		//Prevent crushing
		me.targetEl.setWidth(20000);
    },

    /**
     * This method is called to (re)cache our understanding of flexes. This happens during beginLayout and may need to
     * be called again if the flexes are changed during the layout (e.g., like ColumnLayout).
     * @param {Object} ownerContext
     * @protected
     */

    cacheFlexes: function (ownerContext) {
        var me = this,
			vtotalFlex = 0,
			htotalFlex = 0,
            childItems = ownerContext.childItems,
            i = childItems.length,
            vflexedItems = [],
            hflexedItems = [],
            child, childContext, flex;
        while (i--) {
            childContext = childItems[i];
            child = childContext.target;

			childContext.vflex = flex = (child.height === 'flex') ? (child.vweight || 1) : 0;
			if (flex) {
				vtotalFlex += flex;
				vflexedItems.push(childContext);
			}
			
			childContext.hflex = flex = (child.width === 'flex') ? (child.hweight || 1) : 0;
			if(flex) {
				htotalFlex += flex;
				hflexedItems.push(childContext);
			}
        }
        ownerContext.vflexedItems = vflexedItems;
        ownerContext.vtotalFlex = vtotalFlex;

        ownerContext.hflexedItems = hflexedItems;
        ownerContext.htotalFlex = htotalFlex;
    },

    calculate: function(ownerContext) {
        var me = this,
            targetSize = me.getContainerSize(ownerContext),
            state = ownerContext.state,
            plan = state.boxPlan || (state.boxPlan = {});
	
        plan.targetSize = targetSize;

        if (!state.parallelDone) {
			//me.printBoxes(ownerContext,["before parallel",state.parallelDone]);
            state.parallelDone = me.calculateParallel(ownerContext, plan);
			//me.printBoxes(ownerContext,["after parallel",state.parallelDone]);
        }

        if (!state.perpendicularDone) {
			//me.printBoxes(ownerContext,["before perpendicular",state.perpendicularDone]);
            state.perpendicularDone = me.calculatePerpendicular(ownerContext, plan);
			//me.printBoxes(ownerContext,["after perpendicular",state.perpendicularDone]);
        }
        if (state.parallelDone && state.perpendicularDone) {
            if (me.done && !state.flexedDone) {
				me.reCalculateFlexed(ownerContext, plan);
                state.flexedDone = true;
            }
            me.publishInnerCtSize(ownerContext, me.reserveOffset ? me.availableSpaceOffset : 0);

			me.overflowHandler.calculate(ownerContext);

        } else {
            me.done = false;
        }
    },
    calculateParallel: function(ownerContext, plan) {
        var me = this,
            shrinkWrap = me.horizontal ? ownerContext.widthModel.shrinkWrap : ownerContext.heightModel.shrinkWrap,
            childItems = ownerContext.childItems,
            childItemsLength = childItems.length,
            flexedItems = me.horizontal ? ownerContext.hflexedItems : ownerContext.vflexedItems,
            flexedItemsLength = flexedItems.length,
			isCenter = me.horizontal ? ownerContext.halign.center : ownerContext.valign.middle,
			isEnd = me.horizontal ? ownerContext.halign.right : ownerContext.valign.bottom,
            padding = me.padding,
            start = padding[me.horizontal ? 'left':'top'],
            nonFlexSize = start + padding[me.horizontal ? 'right':'bottom'],
            i, childMargins, childSize, childMinSize, remainingSize, remainingFlex, childContext, flex, flexedSize, contentSize;


        // Determine the size used by non-flexed items and
		// the minimum size of items that have their minWidth/minHeight set to 'wrap':
        for (i = 0; i < childItemsLength; ++i) {
            childContext = childItems[i];
            childMargins = childContext.marginInfo || childContext.getMarginInfo();

            nonFlexSize += me.horizontal ? childMargins.width : childMargins.height;

			if(childContext[me.horizontal ? 'hflex':'vflex']) { //Flexible item -> determine minimum size
				if(childContext.target[me.horizontal ? 'minWidth':'minHeight'] === 'wrap') {
					childMinSize = childContext.props[me.horizontal ? 'minWrapWidth':'minWrapHeight'];
					//We don't have the minimum-dimension property yet, so measure by asking for the dimension property
					if(isNaN(childMinSize)) {
						childMinSize = childContext.getProp(me.horizontal ? 'width':'height');
						if (isNaN(childMinSize)) {
							return false;
						} else {
							childContext.props[me.horizontal ? 'minWrapWidth':'minWrapHeight'] = childMinSize;
						}
					}
				}
			} else { // Non-flexible item -> determine size
				childSize = childContext.getProp(me.horizontal ? 'width':'height'); 
				if (isNaN(childSize)) {
					return false;
				}
				nonFlexSize += childSize;
			}
        }


        // If we get here, we have all the parallel sizes for non-flexed items and minimum sizes of the flexible ones...
        if (shrinkWrap) {
            plan.availableSpace = 0;
        } else {
            plan.availableSpace = (me.horizontal ? plan.targetSize.width : plan.targetSize.height) - nonFlexSize;
        }


		//Sort the flexed items by their minimum size
		//To make sure we determine the set the size of the largest minimum size first.
		//If we don't we might use up to much of the available space too early, and not have enough left for the later ones
       	Ext.Array.sort(flexedItems, me.flexSort);

        contentSize = nonFlexSize;
        remainingSize = plan.availableSpace;
        remainingFlex = me.horizontal ? ownerContext.htotalFlex : ownerContext.vtotalFlex;
	
	    // Calculate flexed item sizes:
        for (i = 0; i < flexedItemsLength; i++) {

            childContext = flexedItems[i];
            flex         = childContext[me.horizontal ? 'hflex':'vflex'];

            flexedSize   = me.roundFlex((flex / remainingFlex) * remainingSize);

			//Make sure that we respect the minimum size 
			if(childContext.target[me.horizontal ? 'minWidth':'minHeight'] === 'wrap') {
				childMinSize = childContext.props[me.horizontal ? 'minWrapWidth':'minWrapHeight'];
			} else {
				childMinSize = parseInt(childContext.target[me.horizontal ? 'minWidth':'minHeight']) || 0;
			}

			flexedSize	= Math.max(flexedSize,childMinSize);

			//Store flexed size as property and set the dimension if this layout is responsible for sizing it
			childContext.setProp(me.horizontal ? 'flexedWidth':'flexedHeight',flexedSize);
			if(childContext[me.horizontal ? 'widthModel':'heightModel'].calculated) {
				childContext[me.horizontal ? 'setWidth':'setHeight'](flexedSize);
			}

            // for shrinkWrap w/flex, the item will be reduced to minWidth (maybe 0)
            // due to minWidth/minHeight constraints, it may be that flexedSize > remainingSize
            contentSize += flexedSize;
            // Remaining space has already had margins subtracted, so just subtract size
            remainingSize = Math.max(0, remainingSize - flexedSize); // no negatives!
            remainingFlex -= flex;
        }
		// Check if we have all the information needed to position the items:
		// If we are not shrinkWrap in the parallel dimension, we need its size before we can lay out boxes
        if (me.horizontal ? (!ownerContext.widthModel.shrinkWrap && !plan.targetSize.gotWidth)
						  : (!ownerContext.heightModel.shrinkWrap && !plan.targetSize.gotHeight)) {
            return false;
        }

        if (isCenter) {
            start += remainingSize / 2;

            // If content is too wide to pack to center, do not allow the centering calculation to place it off the left edge.
            if (start < 0) {
                start = 0;
            }
        } else if (isEnd) {
            start += remainingSize;
        }

        // Assign parallel position for the boxes:
        for (i = 0; i < childItemsLength; ++i) {
            childContext = childItems[i];
			childMargins = childContext.marginInfo;

            start += childMargins[me.horizontal ? 'left':'top'];

            childContext.setProp(me.horizontal ? 'x':'y', start);

			//Use flexWidth/flexHeight property for flexible items to figure out how big they ought to be
			//(and will be after invalidation)
			childSize = me.horizontal ? childContext.props[childContext.hflex ? 'flexedWidth':'width']
									  : childContext.props[childContext.vflex ? 'flexedHeight':'height'];

            start += childMargins[me.horizontal ? 'right':'bottom'] + childSize;
        }

        // Stash the contentSize on the state so that it can always be accessed later in the calculation
        ownerContext.state.contentSize = contentSize + ownerContext.targetContext.getPaddingInfo()[me.horizontal ? 'width':'height'];

		// Publish parallel content size
        ownerContext[me.horizontal ? 'setContentWidth':'setContentHeight'](ownerContext.state.contentSize);

        return true;
    },

    calculatePerpendicular: function(ownerContext, plan) {
        var me = this,
            shrinkWrap = me.horizontal ? ownerContext.heightModel.shrinkWrap : ownerContext.widthModel.shrinkWrap,
            targetSize = plan.targetSize,
            childItems = ownerContext.childItems,
			childItemsLength = childItems.length,
            padding = me.padding,
            start = padding[me.horizontal ? 'top':'left'],
            availSize = me.horizontal ? (targetSize.height - start - padding.bottom) : (targetSize.width - start - padding.right),
            isCenter = me.horizontal ? ownerContext.valign.middle : ownerContext.halign.center,
            isEnd = me.horizontal ? ownerContext.valign.bottom : ownerContext.halign.right,
            maxSize = 0,
            childStart, i, childSize, childMinSize, childMargins, diff, size, childContext, childSizeModel;


		//Unless we are shrinkwrapping we need to know the available size if
		//we want to align or flex items
        if (!shrinkWrap && (isCenter || isEnd || (me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex))) {
            if (isNaN(availSize)) {
                return false;
            }
        }

        // Determine the maximum size of the non-flexible items and
		// the flexible ones that have their minimum set to 'wrap'.
		for (i = 0; i < childItemsLength; i++) {
			childContext = childItems[i];
			childMargins = childContext.marginInfo || childContext.getMarginInfo();
			childSizeModel = childContext[me.horizontal ? 'heightModel':'widthModel'];

			//Only measure the items which determine their own size
			if(childSizeModel.configured || childSizeModel.shrinkWrap) { 
				childSize = childContext.getProp(me.horizontal ? 'height' : 'width');
				maxSize = Math.max(maxSize, childSize + childMargins[me.horizontal ? 'height':'width']);

				if(isNaN(maxSize)) {
					return false;
				}
				//Track the minimal wrap size of flexed items with min size set to 'wrap'
				if(childContext[me.horizontal?'vflex':'hflex'] && childContext.target[me.horizontal ? 'minHeight':'minWidth'] === 'wrap') {
					childContext.props[me.horizontal ? 'minWrapHeight':'minWrapWidth'] = childSize;
				}
			}
		}

		plan.maxSize = maxSize;
		ownerContext[me.horizontal ? 'setContentHeight':'setContentWidth'](maxSize + me.padding[me.horizontal ? 'height' : 'width'] +
			ownerContext.targetContext.getPaddingInfo()[me.horizontal ? 'height' : 'width']);
		
		if (isCenter || isEnd) {		
			// When calculating a centered position within the content box of the innerCt,
			// the width of the borders must be subtracted from the size to yield the
			// space available to center within. The publishInnerCtSize method explicitly
			// adds the border widths to the set size of the innerCt.
			size = shrinkWrap ? maxSize : availSize;
			size = size - ownerContext.innerCtContext.getBorderInfo()[me.horizontal ? 'height' : 'width'];
		}

        for (i = 0; i < childItemsLength; i++) {
            childContext = childItems[i];
            childMargins = childContext.marginInfo || childContext.getMarginInfo();

            childStart = start + childMargins[me.horizontal ? 'top':'left'];
			
			//Size flexible items, position others
			if(childContext[me.horizontal ? 'vflex':'hflex']) {

				//If we are shrinkWrapping all flexible items must become as large as the maximum,
				//otherwise they must flex to fill the available space
				if(shrinkWrap) {
					flexedSize = maxSize - childMargins[me.horizontal ? 'height':'width'];
				} else {
					flexedSize = availSize - childMargins[me.horizontal ? 'height':'width'];
				}

				//Make sure that we respect the minimum size 
				if(childContext.target[me.horizontal ? 'minHeight':'minWidth'] === 'wrap') {
					childMinSize = childContext.props[me.horizontal ? 'minWrapHeight':'minWrapWidth'];
				} else {
					childMinSize = parseInt(childContext.target[me.horizontal ? 'minHeight':'minWeight']) || 0;
				}
				flexedSize = Math.max(flexedSize,childMinSize);

				//Store flexed size as property and set the dimension if this layout is responsible for sizing it
				childContext.setProp(me.horizontal ? 'flexedHeight':'flexedWidth', flexedSize);
				if(childContext[me.horizontal ? 'heightModel':'widthModel'].calculated) {
					childContext[me.horizontal ? 'setHeight':'setWidth'](flexedSize);
				}

            } else if (isCenter || isEnd) {

				if(isNaN(size)) { //If we need to align an element we need the container size first
					return false;
				}
				childSize = me.horizontal ? childContext.props[childContext.vflex ? 'flexedHeight':'height']
										  : childContext.props[childContext.hflex ? 'flexedWidth':'width'];
				if(isCenter) {
                	if ((diff = size - childSize) > 0) {
                    	childStart = start + Math.round(diff / 2);
                	}
				} else if (isEnd) {
					childStart = start + Math.max(0,size - childSize - childMargins[me.horizontal ? 'bottom':'right']);
				}
            } 
            childContext.setProp(me.horizontal ? 'y':'x', childStart);
        }
        return true;
    },

	reCalculateFlexed: function(ownerContext, plan) {
		var me = this,
			childItems = ownerContext.childItems,
            childItemsLength = childItems.length,
			onBeforeInvalidateChild = me.onBeforeInvalidateChild,
			onAfterInvalidateChild = me.onAfterInvalidateChild,
			childContext, props, i, childWidth,childHeight;
		for (i = 0; i < childItemsLength; i++) {
			childContext = childItems[i];
			props = childContext.props;

			//Only consider flexible items
			if(childContext.hflex || childContext.vflex) {

				childWidth = props[childContext.hflex ? 'flexedWidth' : 'width'];
				childHeight = props[childContext.vflex ? 'flexedHeight' : 'height'];

				if(props.width != childWidth || props.height != childHeight) { //Wrong size
					childContext.invalidate({
						before: onBeforeInvalidateChild,
						after: onAfterInvalidateChild,
						layout: me,
						childWidth: childWidth,
						childHeight: childHeight,
						childX: props.x,
						childY: props.y
					});
				}
			}
        }
	},

	onBeforeInvalidateChild: function (childContext, options) {
			if(childContext.hflex) {
				childContext.widthModel = Ext.layout.SizeModel.calculated;
			} 
			if(childContext.vflex) {
				childContext.heightModel = Ext.layout.SizeModel.calculated;
			}
	},

	onAfterInvalidateChild: function (childContext, options) {

		childContext.setProp('x', options.childX);
		childContext.setProp('y', options.childY);
	
       	if (childContext.widthModel.calculated) {
			childContext.setWidth(options.childWidth);
		}
		if (childContext.heightModel.calculated) {
			childContext.setHeight(options.childHeight);
		}
	},
    completeLayout: function(ownerContext) {
        var me = this;
        me.overflowHandler.completeLayout(ownerContext);
    },
    finishedLayout: function(ownerContext) {
		var me = this;

        me.overflowHandler.finishedLayout(ownerContext);
        me.callParent(arguments);

    },
    publishInnerCtSize: function(ownerContext, reservedSpace) {
        var me = this,
			isCenter = me.horizontal ? ownerContext.valign.middle : ownerContext.halign.center,
			isEnd = me.horizontal ? ownerContext.valign.bottom : ownerContext.halign.right,
            padding = me.padding,
            plan = ownerContext.state.boxPlan,
            targetSize = plan.targetSize,
			parallelShrinkWrap = ownerContext[me.horizontal ? 'widthModel':'heightModel'].shrinkWrap,
			perpendicularShrinkWrap = ownerContext[me.horizontal ? 'heightModel':'widthModel'].shrinkWrap,
            scrollbarWidth = Ext.getScrollbarSize()[me.horizontal ? 'height':'width'],
            innerCtContext = ownerContext.innerCtContext,
            innerCtParallel = (parallelShrinkWrap || (plan.availableSpace < 0)
                    ? ownerContext.state.contentSize
                    : targetSize[me.horizontal ? 'width':'height']) - (reservedSpace || 0),
            innerCtPerpendicular;

        if (!perpendicularShrinkWrap && (me.horizontal ? ownerContext.vtotalFlex : ownerContext.htotalFlex)) { //If there are flexed elements, use full space
            innerCtPerpendicular = targetSize[me.horizontal ? 'height':'width'];
        } else { //Make the innerCt as big as necessary
            innerCtPerpendicular = plan.maxSize + (
				me.horizontal ? (padding.top + padding.bottom + innerCtContext.getBorderInfo().height)
							  : (padding.left + padding.right + innerCtContext.getBorderInfo().width));

            if (!perpendicularShrinkWrap && (isCenter || isEnd)) {
                innerCtPerpendicular = Math.max(targetSize[me.horizontal ? 'height':'width'], innerCtPerpendicular);
            }
            //Make room for scrollbar...
            if(plan.availableSpace < 0) {
                innerCtPerpendicular -= scrollbarWidth;
            }
        }

        innerCtContext.setHeight(me.horizontal ? innerCtPerpendicular : innerCtParallel);
        innerCtContext.setWidth(me.horizontal ? innerCtParallel : innerCtPerpendicular);

        // If unable to publish both dimensions, this layout needs to run again
        if (isNaN(innerCtParallel + innerCtPerpendicular)) {
            me.done = false;
        }
    },

    onRemove: function(comp){
        var me = this;
        me.callParent(arguments);
        if (me.overflowHandler) {
            me.overflowHandler.onRemove(comp);
        }
        if (comp.layoutMarginCap == me.id) {
            delete comp.layoutMarginCap;
        }
    },

    /**
     * @private
     */
    initOverflowHandler: function() {
        var me = this,
            handler = me.overflowHandler,
            handlerType,
            constructor;

        if (typeof handler == 'string') {
            handler = {
                type: handler
            };
        }

        handlerType = 'None';
        if (handler && handler.type !== undefined) {
            handlerType = handler.type;
        }

        constructor = Ext.layout.container.boxOverflow[handlerType];
        if (constructor[me.type]) {
            constructor = constructor[me.type];
        }

        me.overflowHandler = Ext.create('Ext.layout.container.boxOverflow.' + handlerType, me, handler);
    },

    // Overridden method from Ext.layout.container.Container.
    // Used in the beforeLayout method to render all items into.
    getRenderTarget: function() {
        return this.targetEl;
    },

    // Overridden method from Ext.layout.container.Container.
    // Used by Container classes to insert special DOM elements which must exist in addition to the child components

    getElementTarget: function() {
        return this.innerCt;
    },

    /**
     * @private
     */
    destroy: function() {
        Ext.destroy(this.innerCt, this.overflowHandler);
        this.callParent(arguments);
    },

	getRenderData: function() {
		var data = this.callParent();

		data.targetElCls = this.targetElCls;

		return data;
	},
	//UTIL

    flexSort: function (a, b) { //(This is actually a static method)
		var me = this, aMin, bMin;

		if(a.target[me.horizontal ? 'minWidth':'minHeight'] === 'wrap') {
			aMin = a.props[me.horizontal ? 'minWrapWidth':'minWrapHeight'];
		} else {
			aMin = parseInt(a.target[me.horizontal ? 'minWidth':'minHeight']) || 0;
		}
		if(b.target[me.horizontal ? 'minWidth':'minHeight'] === 'wrap') {
			bMin = b.props[me.horizontal ? 'minWrapWidth':'minWrapHeight'];
		} else {
			bMin = parseInt(b.target[me.horizontal ? 'minWidth':'minHeight']) || 0;
		}
        return bMin - aMin;
    },

    roundFlex: function(width) {
        return Math.ceil(width);
    },

	printBoxes: function(ownerContext,extra) {
		var childItems = ownerContext.childItems,
			childItemsLength = childItems.length,
			boxes = [],
			i;

		for(i = 0; i < childItemsLength; i++) {
			boxes[i] = {width: childItems[i].props.width, height: childItems[i].props.height, contentWidth: childItems[i].props.contentWidth, contentHeight: childItems[i].props.contentHeight};	
		}
		console.log("Boxes of", ownerContext.target.id, boxes,extra);
	}
});
