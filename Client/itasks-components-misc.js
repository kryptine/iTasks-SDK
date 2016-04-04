itasks.itwc_splitter = {

    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.direction = (me.parentCmp && me.parentCmp.direction) || 'vertical';
        me.vertical = me.direction == 'vertical';
        el.classList.add(me.cssPrefix + me.vertical ? 'vsplitter' : 'hsplitter');
    },
    initSize: function() {
    },
    initMargins: function() {
    },
    afterAdd: function() {
        var me = this,
            el = me.domEl;

        me.prev = el.previousSibling;
        me.next = el.nextSibling;

        el.addEventListener('mousedown', me.onStartDrag.bind(me));
    },
    onStartDrag: function(e) {
        var me = this;

        e.preventDefault();
        me.lastPos = me.vertical ? e.clientY : e.clientX;
        me.onDragging_ = me.onDragging.bind(me);
        me.onStopDrag_ = me.onStopDrag.bind(me);
        window.addEventListener('mousemove', me.onDragging_);
        window.addEventListener('mouseup', me.onStopDrag_);
    },
    onDragging: function(e) {
        var me = this,
            vertical = me.vertical,
            newPos = vertical ? e.clientY : e.clientX,
            sizePrev, sizeNext, sizeDiff = newPos - me.lastPos;

        sizePrev = document.defaultView.getComputedStyle(me.prev,'').getPropertyValue(vertical ? 'height':'width');
        sizeNext = document.defaultView.getComputedStyle(me.next,'').getPropertyValue(vertical ? 'height':'width');
        sizePrev = ((sizePrev,10) + sizeDiff) | 0;
        sizeNext = ((sizeNext,10) - sizeDiff) | 0;
        me.prev.style[vertical ? 'height' : 'width'] = sizePrev + 'px';
        me.next.style[vertical ? 'height' : 'width'] = sizeNext + 'px';

        me.lastPos = newPos;
    },
    onStopDrag: function(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
};
