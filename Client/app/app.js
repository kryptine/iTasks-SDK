Ext.application({
    //Core controller that syncs with server-side session state
    controllers: ["Controller"],

    views: ["Main"],

    requires: ['itwc.patch.layout.Context'],
    //requires: ['itwc.patch.grid.View'
              //,'itwc.patch.layout.Context'],

    name: 'itwc',

    //autoCreateViewport: true

    //On launch, create the viewport
    launch: function() {
        Ext.create('itwc.container.Viewport');
    }
});
