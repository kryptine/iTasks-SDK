/*
* A Container that is wrappable checks for the 'vwrap' and 'hwrap' and 'direction' attributes to
* calculate its size. When 'vwrap' is true and 'direction' is 'vertical', the size of the container
* is set to the sum of the 'height' or 'minHeight' properties of all child elements. 'minHeight' is
* used instead of 'height' when 'vflex' is set for an item. If 'direction' is 'horizontal' and 'vwrap'
* is true, then the height is set to the maximum 'height' or 'minHeight'of all items. When 'hwrap' is 
* set, the width is computed in the same way.
*
* Hint: it is easy to think of wrappable containers as having a rubber band around its items.
*/
Ext.define('itasks.mixin.Wrappable',{

	//Flag the component as wrappable so that layouts can test for it.
	wrappable: true,

	/*
	* Resize this component to its wrap size
	*/
	wrap: function() {
		var me = this,
			wrapSize;
		
		//No need to wrap if the component is not rendered yet
		if(!me.rendered) {
			return;
		}	
		wrapSize = me.getWrapSize();
		if(wrapSize.width && wrapSize.height) {
			me.setSize(wrapSize.width,wrapSize.height);
		} else {
			if(wrapSize.width)
				me.setWidth(wrapSize.width);
			if(wrapSize.height)
				me.setHeight(wrapSize.height);
		}
	},
	/*
	* Compute this components wrap size based on it's child items.
	*/
	getWrapSize: function() {
		var me = this,
			wrapWidth = 0,
			wrapHeight = 0,
			visibleItems,
			dockItems,
			
			i, length, child, childSize, childMargins, dock, compFnWidth, compFnHeight,
			undefinedValue;
		
		//Depending on the direction of this container we either have
		//to sum or max the values
		if(me.layout.direction == 'horizontal') {
			compFnWidth	= me.sumSizes;
			compFnHeight = me.maxSizes;
		} else {
			compFnWidth = me.maxSizes;
			compFnHeight = me.sumSizes;
		}
			
		visibleItems = me.layout.getVisibleItems();	
		
		//Compute the sizes of the child elements in the container
		for(i = 0, length = visibleItems.length; i < length; i++) {
			child = visibleItems[i];
			
			//If the child is wrappable itself, we use its wrap size rather than its current size
			if(child.wrappable) {
				childSize = child.getWrapSize();
			} else {
				childSize = child.getSize();
			}
			//To get a good wrap size, we also need to count the space required for the margins
			if(child.margins) {
				childMargins = child.margins;
				childSize = {width: childSize.width + childMargins.left + childMargins.right
							,height: childSize.height + childMargins.top + childMargins.bottom
							};
			}
						
			wrapWidth = compFnWidth(wrapWidth, childSize.width);
			wrapHeight = compFnHeight(wrapHeight, childSize.height); 
		}
		//When this container uses a dock layout, we also need to count the sizes of the
		//docked items
		if(me.componentLayout.type == 'dock') {
			dockItems = me.componentLayout.getLayoutItems();
			
			//Sort the dock items to make sure that the 'top' docked items are
			//processed last because these items span both the main panel, as well as
			//the left and right docked items. If we don't sort, the width of left
			//and right documented items is counted twice.
			Ext.Array.sort(dockItems,function(a,b) {
				return (a.dock == 'top' ? 1 : -1);
			});
			
			length = dockItems.length;
			for(i = 0; i < length; i++) {
				dock = dockItems[i];
				
				if(dock.dock == 'left' || dock.dock == 'right') {
					wrapWidth += dock.getWidth();
					wrapHeight = Math.max(wrapHeight,dock.getHeight());
				} else if(dock.dock == 'top' || dock.dock == 'bottom') {
					wrapWidth = Math.max(wrapWidth, dock.getWidth())
					wrapHeight += dock.getHeight();
				} 
			}
		}
		
		//Add the padding of this element to the wrap sizes
		if(me.padding) {
			if(typeof me.padding != 'object') {
				me.padding = Ext.util.Format.parseBox(me.padding);
			}
			wrapWidth += me.padding.left + me.padding.right;
			wrapHeight += me.padding.top + me.padding.bottom;
		}
		//Add the framesize of this element to the wrap size
		//HACK: these values should not be fixed, but inferred somehow...
		//me.frameSize does not contain the right values yet when getWrapSize is called...
		if(me.frame) {
			wrapWidth += 10;
			wrapHeight += 10;
		}
		wrapWidth = me.hwrap ? wrapWidth : me.width; 
		wrapHeight = me.vwrap ? wrapHeight : me.height;
		
		return {width: wrapWidth, height: wrapHeight};
	},
	sumSizes: function(oldv, newv) {
		return oldv + (newv || 0);
	},
	maxSizes: function(oldv, newv) {
		return Math.max(oldv, newv || 0);
	}
});
