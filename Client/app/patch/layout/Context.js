/**
* Context with patched run method with bigger watchDog value
*/
Ext.define('itwc.patch.layout.Context', {
	override: 'Ext.layout.Context',
    /**
     * Runs the layout calculations. This can be called only once on this object.
     * @return {Boolean} True if all layouts were completed, false if not.
     */
    run: function () {
        var me = this,
            flushed = false,
// BEGIN PATCH
            watchDog = 500;
// END PATCH

        me.flushInvalidates();

        me.state = 1;
        me.totalCount = me.layoutQueue.getCount();

        // We may start with unflushed data placed by beginLayout calls. Since layouts may
        // use setProp as a convenience, even in a write phase, we don't want to transition
        // to a read phase with unflushed data since we can write it now "cheaply". Also,
        // these value could easily be needed in the DOM in order to really get going with
        // the calculations. In particular, fixed (configured) dimensions fall into this
        // category.
        me.flush();

        // While we have layouts that have not completed...
        while ((me.remainingLayouts || me.invalidQueue.length) && watchDog--) {
            if (me.invalidQueue.length) {
                me.flushInvalidates();
            }

            // if any of them can run right now, run them
            if (me.runCycle()) {
                flushed = false; // progress means we probably need to flush something
                // but not all progress appears in the flushQueue (e.g. 'contentHeight')
            } else if (!flushed) {
                // as long as we are making progress, flush updates to the DOM and see if
                // that triggers or unblocks any layouts...
                me.flush();
                flushed = true; // all flushed now, so more progress is required

                me.flushLayouts('completionQueue', 'completeLayout');
            } else {
                // after a flush, we must make progress or something is WRONG
                me.state = 2;
                break;
            }

            if (!(me.remainingLayouts || me.invalidQueue.length)) {
                me.flush();
                me.flushLayouts('completionQueue', 'completeLayout');
                me.flushLayouts('finalizeQueue', 'finalizeLayout');
            }
        }

        return me.runComplete();
    }
});
