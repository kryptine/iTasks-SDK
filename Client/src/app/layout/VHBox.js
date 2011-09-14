Ext.define('itasks.layout.VHBox',{

	extend: 'Ext.layout.container.Box',
	alias: 'layout.vhbox',
	type: 'vhbox',
	
	direction: 'vertical',
	
	constructor: function(config) {
		
		if(config.direction == 'horizontal') {
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
	calculateChildBoxes: function(visibleItems, targetSize) {
		
		var me = this,
			math = Math,
			mmax = Math.max,
			infiniteValue = Infinity,
			undefinedValue,
			
			parallelPrefix = me.parallelPrefix,
			parallelPrefixCap = me.parallelPrefixCap,
			perpendicularPrefix = me.perpendicularPrefix,
			perpendicularPrefixCap = me.perpendicularPrefixCap,
			parallelMinString = 'min' + parallelPrefixCap,
			perpendicularMinString = 'min' + perpendicularPrefixCap,
			perpendicularMaxString = 'max' + perpendicularPrefixCap,
			
			parallelSize = targetSize[parallelPrefix] - me.scrollOffset,
			perpendicularSize = targetSize[perpendicularPrefix],
			padding = me.padding,
			parallelOffset = padding[me.parallelBefore],
			paddingParallel = parallelOffset + padding[me.parallelAfter],
			perpendicularOffset = padding[me.perpendicularLeftTop],
			paddingPerpendicular = perpendicularOffset + padding[me.perpendicularRightBottom],
			availPerpendicularSize = mmax(0, perpendicularSize - paddingPerpendicular),
			
			innerCtBorderWidth = me.innerCt.getBorderWidth(me.perpendicularLT + me.perpendicularRB),
			
			isStart =	me.parallelAlign == 'left' || me.parallelAlign == 'top', //Allow left and top as valid values for both halign and valign
			isCenter =  me.parallelAlign == 'center' || me.parallelAlign == 'middle',
			isEnd =		me.parallelAlign == 'right' || me.parallelAlign == 'bottom',
			
			constrain = Ext.Number.constrain,
			visibleCount = visibleItems.length,
			nonFlexSize = 0,
			totalFlex = 0,
			desiredSize = 0,
			minimumSize = 0,
			maxSize = 0,
			boxes = [],
			minSizes = [],
			calculatedWidth,
			
			i, child, childParallel, childPerpendicular, childMargins, childSize, minParallel, tmpObj, shortfall,
			tooNarrow, availableSpace, minSize, item, length, itemIndex, box, oldSize, newSize, reduction, diff,
			flexedBoxes, remainingSpace, remainingFlex, flexedSize, parallelMargins, calcs, offset,
			perpendicularMargins, stretchSize;
		
		for(i = 0; i < visibleCount; i++) {
			child = visibleItems[i];
			
			if(!child[me.parallelFlex] || !child[me.perpendicularFlex]) {
				if(child.componentLayout.initialized !== true) {
					me.layoutItem(child);
				}
			}
			
			childMargins = child.margins;
			parallelMargins = childMargins[me.parallelBefore] + childMargins[me.parallelAfter];
			
			tmpObj = {
				component: child,
				margins: childMargins
			};
			
			//Here we compute the wrap size of wrappable childs.
			//The wrap size is used as minimum size for flexible childs and as the fixed
			//size of all other items.
			if(child.wrappable) {
				wrapSize = child.getWrapSize();
			} else { 
				wrapSize = {width: undefinedValue, height: undefinedValue};
			}
			
			if(child[me.parallelFlex]) {
				totalFlex += child[me.parallelFlex];
				childParallel = undefinedValue;
				childPerpendicular = wrapSize[perpendicularPrefix] || child[perpendicularPrefix]
			} else {
				if(!(child[parallelPrefix] && child[perpendicularPrefix])) {
					childSize = child.getSize();
				}
				childParallel = wrapSize[parallelPrefix] || child[parallelPrefix] || childSize[parallelPrefix];
				childPerpendicular = wrapSize[perpendicularPrefix] || child[perpendicularPrefix] || childSize [perpendicularPrefix];
			}
			
			nonFlexSize += parallelMargins + (childParallel || 0);
			desiredSize += parallelMargins + (child[me.parallelFlex] ? child[parallelMinString] || 0 : childParallel);
			minimumSize += parallelMargins + (child[parallelMinString] || childParallel || 0);
			
			if(!child[me.perpendicularFlex] && typeof childPerpendicular != 'number') {
				childPerpendicular = child['get' + perpendicularPrefixCap]();
			}
		
			maxSize = mmax(maxSize, mmax(childPerpendicular, child[perpendicularMinString]||0) + childMargins[me.perpendicularLeftTop] + childMargins[me.perpendicularRightBottom]);
			
			tmpObj[parallelPrefix] = childParallel || undefinedValue;
			tmpObj[perpendicularPrefix] = childPerpendicular || undefinedValue;
			//The child has a 'dirty' size if it has changed since it was previously layed out or
			//when the child is initially wrappable
			if(child.componentLayout.lastComponentSize) {
				tmpObj.dirtySize = tmpObj[parallelPrefix] !== child.componentLayout.lastComponentSize[parallelPrefix];
				if(!tmpObj.dirtySize) {
					tmpObj.dirtySize = tmpObj.dirtySize || (wrapSize[perpendicularPrefix] > 0);
				}
			} else {
				tmpObj.dirtySize = ((wrapSize[parallelPrefix] > 0) || (wrapSize[perpendicularPrefix] > 0));
			}
			boxes.push(tmpObj);
		}
		
		if(!me.autoSize) {
			shortfall = desiredSize - parallelSize;
			tooNarrow = minimumSize > parallelSize;
		}
		
		availableSpace = mmax(0, parallelSize - nonFlexSize - paddingParallel - (me.reserveOffset ? me.availableSpaceOffset : 0));
		
		if(tooNarrow) {
			for(i = 0; i < visibleCount; i++) {
				box = boxes[i];
				minSize = visibleItems[i][parallelMinString] || box[parallelPrefix];
				box.dirtySize = box.dirtySize || box[parallelPrefix] != minSize;
				box[parallelPrefix] = minSize;
			}
		} else {
			if(shortfall > 0) {
				for(i = 0; i < visibleCount; i++) {
					item = visibleItems[i];
					minSize = item[parallelMinString] || 0;
					
					if(item[me.parallelFlex]) {
						box = boxes[i];
						box.dirtySize = box.dirtySize || box[parallelPrefix] != minSize;
						box[parallelPrefix] = minSize;
					} else if(me.shrinkToFit) {
						minSizes.push({
							minSize: minSize,
							available: boxes[i][parallelPrefix] - minSize,
							index: i
						});
					}
				}
				
				Ext.Array.sort(minSizes, me.minSizeSortFn);
				
				for(i = 0, length = minSizes.length; i < length; i++) {
					itemIndex = minSizes[i].index;
					
					if(itemIndex == undefinedValue) {
						continue;
					}
					item = visibleItems[itemIndex];
					minSize = minSizes[i].minSize;
					
					box = boxes[itemIndex];
					oldSize = box[parallelPrefix];
					newSize = mmax(minSize, oldSize - math.ceil(shortfall / (length - i)));
					reduction = oldSize - newSize;
					
					box.dirtySize = box.dirtySize || box[parallelPrefix] != newSize;
					box[parallelPrefix] = newSize;
					shortfall -= reduction;
				}
				tooNarrow = (shortfall > 0);
			} else {
				remainingSpace = availableSpace;
				remainingFlex = totalFlex;
				flexedBoxes = [];
				
				for(i = 0; i < visibleCount; i++) {
					child = visibleItems[i];
					if(child[me.parallelFlex]) {
						flexedBoxes.push(boxes[i]);
					}
				}
				
				Ext.Array.sort(flexedBoxes, me.flexSortFn);
				
				for(i = 0; i < flexedBoxes.length; i++) {
					calcs = flexedBoxes[i];
					child = calcs.component;
					childMargins = calcs.margins;
					
					flexedSize = math.ceil((child[me.parallelFlex] / remainingFlex) * remainingSpace);
					
					flexedSize = Math.max(child['min' + parallelPrefixCap] || 0, math.min(child['max' + parallelPrefixCap] || infiniteValue, flexedSize));
					
					remainingSpace -= flexedSize;
					remainingFlex -= child[me.parallelFlex];
				
					calcs.dirtySize = calcs.dirtySize || calcs[parallelPrefix] != flexedSize;
					calcs[parallelPrefix] = flexedSize;				
				}
			}
		}
		//Determine where to start laying out the items
		//Only move the offset if there are no flexed items to fill up the available space
		if(totalFlex == 0) {
			if(isCenter) {
				parallelOffset += availableSpace / 2;
			} else if(isEnd) {
				parallelOffset += availableSpace;
			}
		}		
		//Scary fix for dock layouts
		if(me.owner.dock && (Ext.isIE6 || Ext.isIE7 || Ext.isIEQuirks) && !me.owner.width && me.direction == 'vertical') {
			calculatedWidth = maxSize + me.owner.el.getPadding('lr') + me.owner.el.getBorderWidth('lr');
			if(me.owner.frameSize) {
				calculatedWidth += me.owner.frameSize.left + me.owner.frameSize.right;
			}
			availPerpendicularSize = Math.min(availPerpendicularSize, targetSize.width = maxSize +  padding.left + padding.right);
		}
		
		for(i = 0; i < visibleCount; i++) {
			child = visibleItems[i];
			calcs = boxes[i];
			
			childMargins = calcs.margins;
			
			perpendicularMargins = childMargins[me.perpendicularLeftTop] + childMargins[me.perpendicularRightBottom];
			
			parallelOffset += childMargins[me.parallelBefore];
			
			calcs[me.parallelBefore] = parallelOffset;
			calcs[me.perpendicularLeftTop] = perpendicularOffset + childMargins[me.perpendicularLeftTop];
			
			//Stretch items which also have a flex weight set in the perpendicular direction
			if(child[me.perpendicularFlex]) {
				stretchSize = constrain(availPerpendicularSize - perpendicularMargins, child[perpendicularMinString] || 0, child[perpendicularMaxString] || infiniteValue);
				calcs.dirtySize = calcs.dirtySize || calcs[perpendicularPrefix] != stretchSize;
				calcs[perpendicularPrefix] = stretchSize;
			} else if (me.perpendicularAlign == 'center' || me.perpendicularAlign == 'middle') {
				diff = mmax(availPerpendicularSize, maxSize) - innerCtBorderWidth - calcs[perpendicularPrefix];
				if(diff > 0) {
					calcs[me.perpendicularLeftTop] = perpendicularOffset + Math.round(diff / 2);
				}
			} else if (me.perpendicularAlign == 'right' || me.perpendicularAlign == 'bottom') {
				diff = mmax(availPerpendicularSize, maxSize) - innerCtBorderWidth - calcs[perpendicularPrefix];
				if(diff > 0) {
					calcs[me.perpendicularLeftTop] = perpendicularOffset + diff;
				}
			}
			
			parallelOffset += (calcs[parallelPrefix] || 0) + childMargins[me.parallelAfter];
				
		}
		return {
			boxes: boxes,
			meta: {
				calculatedWidth: calculatedWidth,
				maxSize: maxSize,
				nonFlexSize: nonFlexSize,
				desiredSize: minimumSize,
				shortfall: shortfall,
				tooNarrow: tooNarrow
			}
		};
	},	
	updateInnerCtSize: function(tSize, calcs) {
        var me = this,
            mmax = Math.max,
            align = me.align,
            padding = me.padding,
            width = tSize.width,
            height = tSize.height,
            meta = calcs.meta,
            innerCtWidth,
            innerCtHeight;
		
		innerCtWidth = width;
		innerCtHeight = height;

		/*
		if (me.direction == 'horizontal') {
			
			innerCtWidth = width;
            innerCtHeight = meta.maxSize + padding.top + padding.bottom + me.innerCt.getBorderWidth('tb');

            if (align == 'stretch') {
                innerCtHeight = height;
            }
            else if (align == 'middle') {
                innerCtHeight = mmax(height, innerCtHeight);
            }
  
        } else {
            innerCtHeight = height;
            innerCtWidth = innerCtWidth
            innerCtWidth = meta.maxSize + padding.left + padding.right + me.innerCt.getBorderWidth('lr');
            
            if (align == 'stretch') {
                innerCtWidth = width;
            }
            else if (align == 'center') {
                innerCtWidth = mmax(width, innerCtWidth);
            }
        
        }
        */	
        me.getRenderTarget().setSize(innerCtWidth || undefined, innerCtHeight || undefined);

        // If a calculated width has been found (and this only happens for auto-width vertical docked Components in old Microsoft browsers)
        // then, if the Component has not assumed the size of its content, set it to do so.
        if (meta.calculatedWidth && me.owner.el.getWidth() > meta.calculatedWidth) {
            me.owner.el.setWidth(meta.calculatedWidth);
        }
		
        if (me.innerCt.dom.scrollTop) {
            me.innerCt.dom.scrollTop = 0;
        }
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
		
		this.callParent(arguments);
	},
	renderItem: function(item) {
		this.callParent(arguments);
	},
	destroy: function() {
		this.callParent(arguments);	
	}
});
