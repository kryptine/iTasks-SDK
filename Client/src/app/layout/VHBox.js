Ext.define('itasks.layout.VHBox',{

	extend: 'Ext.layout.container.Box',
	alias: 'layout.vhbox',
	type: 'vhbox',
	direction: 'vertical',

	constructor: function(config) {
		
		if(config.direction == 'horizontal') {
			this.direction = 'horizontal';
			this.parallelSizeIndex = 0;
			this.perpendicularSizeIndex = 1;
			
			this.parallelPrefix = 'width';
			this.parallelPrefixCap = 'Width';
			this.parallelLT = 'l';
			this.parallelRB = 'r';
			this.parallelBefore = 'left';
			this.parallelBeforeCap = 'Left';
			this.parallelAfter = 'right';
			this.parallelPosition = 'x';
			this.parallelFlex = 'hflex'
			this.parallelAlign = config['halign'] || 'center';
			
			this.perpendicularPrefix = 'height';
			this.perpendicularPrefixCap = 'Height';
			this.perpendicularLT = 't';
			this.perpendicularRB = 'b';
			this.perpendicularLeftTop = 'top';
			this.perpendicularRightBottom = 'bottom';
			this.perpendicularPosition = 'y';
			this.perpendicularFlex = 'vflex';
			this.perpendicularAlign = config['valign'] || 'middle';
		} else {
			this.direction = 'vertical';
			this.parallelSizeIndex = 0;
			this.perpendicularSizeIndex = 1;
			
			this.parallelPrefix = 'height';
			this.parallelPrefixCap = 'Height';
			this.parallelLT = 't';
			this.parallelRB = 'b';
			this.parallelBefore = 'top';
			this.parallelBeforeCap = 'Top';
			this.parallelAfter = 'bottom';
			this.parallelPosition = 'y';
			this.parallelFlex = 'vflex';
			this.parallelAlign = config['valign'] || 'top';
			
			this.perpendicularPrefix = 'width';
			this.perpendicularPrefixCap = 'Width';
			this.perpendicularLT = 'l';
			this.perpendicularRB = 'r';
			this.perpendicularLeftTop = 'left';
			this.perpendicularRightBottom = 'right';
			this.perpendicularPosition = 'x';
			this.perpendicularFlex = 'hflex';
			this.perpendicularAlign = config['halign'] || 'center';
		}

		this.callParent(arguments);
	},
	initLayout: function() {
		this.callParent(arguments);

		//Set overflow (can also be done by onRender)
		this.getTarget().setStyle({overflow: 'auto'});
	},
	onLayout: function() {
		// Clear the innerCt size so it doesn't influence the child items.
		if (this.adjustmentPass !== true) {
			this.innerCt.setSize(null, null);
        	}

		var	me = this,
			targetSize = me.getLayoutTargetSize(),
			items = me.getVisibleItems(),
			calcs = me.calculateChildBoxes(items, targetSize),
			boxes = calcs.boxes,
			resized = false;

		//Update the size of the container if it is wrapping and currently
		//too small, or has dimensions determined by unrestricted html flow
		resized = me.updateOwnerSize(targetSize,calcs);
		//If the container was resized, redo the box calculations
		if(resized) {
			targetSize = me.getLayoutTargetSize();
			calcs = me.calculateChildBoxes(items,targetSize);
			boxes = calcs.boxes;
		}

		me.layoutTargetLastSize = targetSize;
		me.childBoxCache = calcs;

		me.updateInnerCtSize(targetSize, calcs);
		me.updateChildBoxes(boxes);
	
		//If the owner was not resized to make things fit, scrollbars appeared/disappeared
		if(!resized) {
			me.handleTargetOverflow(targetSize);
		}
	},

	calculateChildBoxes: function(visibleItems, targetSize) {
		var	me = this,
			parallelPrefix = me.parallelPrefix,
			parallelPrefixCap = me.parallelPrefixCap,
			perpendicularPrefix = me.perpendicularPrefix,
			perpendicularPrefixCap = me.perpendicularPrefixCap,
			parallelFlex = me.parallelFlex,
			perpendicularFlex = me.perpendicularFlex,
			parallelBefore = me.parallelBefore,
			parallelAfter = me.parallelAfter,
			perpendicularLeftTop = me.perpendicularLeftTop,
			perpendicularRightBottom = me.perpendicularRightBottom,
			boxes = [],
			totalFlex = 0,
			nonFlexSize = 0,
			visibleCount = visibleItems.length,
			padding = me.padding,
			wrapSize = {width: 0, height: 0},
			availSize = {width: targetSize.width - padding.left - padding.right 
			            ,height: targetSize.height - padding.top - padding.bottom},
			childrenSize = {width: 0, height: 0},
			shortfall,
			tooSmall = false,
			parallelOffset = padding[me.parallelBefore],
			perpendicularOffset = padding[me.perpendicularLeftTop],
			paddingPerpendicular = perpendicularOffset + padding[me.perpendicularRightBottom],

			innerCtBorderWidth = me.innerCt.getBorderWidth(me.perpendicularLT + me.perpendicularRB),
			
			i, child, childMargins, childSize, childWrapSize, tmpObj, calcs, diff, availableSpace, remainingSpace, remainingFlex,
			flexSize, flexedSize;

		//IMPORTANT: HACKY BEHAVIOR
		//To be able to measure the difference between the container's size and the space available
		//for rendering child components, we need to make sure that its initial size on render is big enough
		//to render the component fully.
		//For containers  that do not have a fixed initial size, but have a size determined by flex or wrap properties
		//a 'simulated' width and/or height have to be set that is big enough. This is reset if the component is resized on the first layout.
		//
		//If we are using a simulated width/height, we need to treat it as having zero available size
		if(me.owner.simulatedHeight) {
			availSize.height = 0;
		}
		if(me.owner.simulatedWidth) {
			availSize.width = 0;
		}
		
		//Collect size information of the items before flexing is applied. 
		for(i = 0; i < visibleCount; i++) {
			child = visibleItems[i];
	
			if(child.componentLayout.initialized !== true) {
				me.layoutItem(child);
			}
	
			tmpObj = {
				component: child
			};

			//Determine margins of the child
			tmpObj.margins = child.margins || {top: 0, right: 0, bottom: 0, right: 0};

			//Measure current size
			childSize = child.getSize();
			tmpObj.width = childSize.width;
			tmpObj.height = childSize.height;

			//Determine minimum size for both dimensions
			tmpObj.minWidth = child.minWidth || 0; 
			tmpObj.minHeight = child.minHeight || 0;
			
			//Determine total flex and sum of non-flexed items parallel direction
			if (child[parallelFlex]) {
				totalFlex += child[parallelFlex];
				//Set the parallel size to its minimum
				tmpObj[parallelPrefix] = tmpObj['min' + parallelPrefixCap];
				//Count the minimum size as non-flexible space	
				nonFlexSize += tmpObj['min' + parallelPrefixCap];
			} else {
				nonFlexSize += childSize[parallelPrefix];
			}	
			//Margins should always be included in the nonFlexSize, also the margins of flex items
			nonFlexSize += child.margins[parallelBefore] + child.margins[parallelAfter];

			//Add child's contribution to wrapsize
			childWrapSize = child[parallelFlex] ? tmpObj['min' + parallelPrefixCap] : tmpObj[parallelPrefix];
			childWrapSize += child.margins[parallelBefore] + child.margins[parallelAfter];
			wrapSize[parallelPrefix] += childWrapSize;

			childWrapSize = child[perpendicularFlex] ? tmpObj['min' + perpendicularPrefixCap] : tmpObj[perpendicularPrefix];
			childWrapSize += child.margins[perpendicularLeftTop] + child.margins[perpendicularRightBottom];
			wrapSize[perpendicularPrefix] = Math.max(wrapSize[perpendicularPrefix], childWrapSize);

			//Determine if the child has changed since last layout
			tmpObj.dirtySize = true;

			boxes.push(tmpObj);
		}

		//If the wrap size is smaller than the target size, we have overflow
		shortfall = {width: wrapSize.width - availSize.width, height: wrapSize.height - availSize.height};
		tooSmall = (shortfall.width > 0 || shortfall.height > 0);

		//Resize flexible items
		availableSpace = Math.max(0, availSize[parallelPrefix] - nonFlexSize - (me.reserveOffset ? me.availableSpaceOffset : 0) );
		remainingSpace = availableSpace;
		remainingFlex = totalFlex;

		for(i = 0; i < visibleCount; i++) {
			child = visibleItems[i];
			calcs = boxes[i];
			childMargins = calcs.margins;
				
			//Flex according to weight ratio in the parallel direction	
			if(child[parallelFlex]) {
				flexSize = Math.ceil((child[parallelFlex] / remainingFlex) * remainingSpace);
					
				remainingSpace -= flexSize;
				remainingFlex -= child[parallelFlex];
				
				flexedSize = calcs[parallelPrefix] + flexSize;

				calcs[parallelPrefix] = flexedSize;	
			}
			//Flex to full available space in the perpendicular direction
			if(child[perpendicularFlex]) {
				flexedSize = availSize[perpendicularPrefix];
				flexedSize = Math.max(flexedSize, calcs['min' + perpendicularPrefixCap]);

				calcs.dirtySize = calcs.dirtySize || calcs[perpendicularPrefix] != flexedSize;
				calcs[perpendicularPrefix] = flexedSize;
			}
		}
		//Position all child items
		if (me.parallelAlign == 'center' || me.parallelAlign == 'middle') {
			parallelOffset += (remainingSpace / 2);
		} else if (me.parallelAlign == 'right' || me.parallelAlign == 'bottom') {
			parallelOffset += remainingSpace;
		}
		for(i = 0; i < visibleCount; i++) {
			child = visibleItems[i];
			calcs = boxes[i];
			
			childMargins = calcs.margins;

			parallelOffset += childMargins[parallelBefore];
			
			calcs[parallelBefore] = parallelOffset;
			calcs[perpendicularLeftTop] = perpendicularOffset + childMargins[perpendicularLeftTop];

			//If align is center or right we need to add to the offset
			if (me.perpendicularAlign == 'center' || me.perpendicularAlign == 'middle') {
				diff = availSize[perpendicularPrefix] - innerCtBorderWidth - calcs[perpendicularPrefix];
				diff = diff - childMargins[perpendicularLeftTop] - childMargins[perpendicularRightBottom];
				if(diff > 0) {
					calcs[perpendicularLeftTop] = perpendicularOffset + Math.round(diff / 2) + childMargins[perpendicularLeftTop];
				}
			} else if (me.perpendicularAlign == 'right' || me.perpendicularAlign == 'bottom') {
				diff = availSize[perpendicularPrefix] - innerCtBorderWidth - calcs[perpendicularPrefix];
				diff = diff - childMargins[perpendicularLeftTop] - childMargins[perpendicularRightBottom];
				if(diff > 0) {
					calcs[perpendicularLeftTop] = perpendicularOffset + diff;
				}
			}
			//Children size is maximum in perpendicular direction
			childrenSize[perpendicularPrefix] = Math.max(childrenSize[perpendicularPrefix], calcs[perpendicularLeftTop] + calcs[perpendicularPrefix] + childMargins[perpendicularRightBottom]);
	
			parallelOffset = parallelOffset + calcs[parallelPrefix] + childMargins[parallelAfter];
		}
		//Children size in parallel direction is simply the last value of parallelOffset 	
		childrenSize[parallelPrefix] = parallelOffset;

		return {
			boxes: boxes,
			meta: {
				wrapSize: wrapSize,
				childrenSize: childrenSize,
				tooSmall: tooSmall,
				nonFlexSize: nonFlexSize,
			}
		};
	},
	updateInnerCtSize: function(tSize, calcs) {
		var	me = this,
			padding = me.padding,
			meta = calcs.meta,
			innerCtWidth, innerCtHeight;
	
		//Update inner container
		innerCtWidth = meta.childrenSize.width + padding.left + padding.right + me.innerCt.getBorderWidth('lr');
		innerCtHeight = meta.childrenSize.height + padding.top + padding.bottom + me.innerCt.getBorderWidth('tb');

		me.getRenderTarget().setSize(innerCtWidth,innerCtHeight);

        	if (me.innerCt.dom.scrollTop) {
            		me.innerCt.dom.scrollTop = 0;
        	}
	},
	updateOwnerSize: function (tSize, calcs) {
		var	me = this,
			owner = me.owner,
			meta = calcs.meta,
			target = me.getTarget(),
			padding = me.padding,
			resized = false,
			newSize = {width: undefined, height: undefined},
			outerSize, innerSize, diff, minOwnerWidth, minOwnerHeight;

		//Determine the difference between the container target area and the full component
		outerSize = owner.getSize();
		innerSize = target.getStyleSize();

		diff = {width: outerSize.width - innerSize.width, height: outerSize.height - innerSize.height};

		if(owner.hwrap) {
			minOwnerWidth = meta.wrapSize.width + diff.width
					+ padding.left + padding.right + me.innerCt.getBorderWidth('lr');
			owner.wrapWidth = minOwnerWidth;
			owner.minWidth = minOwnerWidth;
			
			//If the owner is too small, or has no width property set yet, set it to the minimal size
			if(tSize.width < meta.wrapSize.width || owner.simulatedWidth) { 
				newSize.width = minOwnerWidth;
				resized = true;
			}
		}
		if(owner.vwrap) {
			minOwnerHeight = meta.wrapSize.height + diff.height
		        		 + padding.top + padding.bottom  + me.innerCt.getBorderWidth('tb');
			owner.wrapHeight = minOwnerHeight;
			owner.minHeight = minOwnerHeight;

			if(tSize.height < meta.wrapSize.height || owner.simulatedHeight) {
				newSize.height = minOwnerHeight;
				resized = true;
			}
		}
		if(resized) {
			owner.setSize(newSize);
		}

		owner.simulatedWidth = false;
		owner.simulatedHeight = false;

		return resized;
	},
	configureItem: function(item) {
		//Set if an item has a height/width managed by this layout
		if(item.hflex) {
			item.layoutManagedWidth = 1;
		} else {
			item.layoutManagedWidth = 2;
		}
		if(item.vflex) {
			item.layoutManagedHeight = 1;
		} else {
			item.layoutManagedHeight = 2;
		}

		//Track resizes of child elements
		this.owner.addManagedListener(item,'resize',this.onChildResized,this);	

		this.callParent(arguments);
	},
	onChildResized: function(child, size) {
		if(!this.layoutBusy) {
			this.onLayout();
		}
	}
});
