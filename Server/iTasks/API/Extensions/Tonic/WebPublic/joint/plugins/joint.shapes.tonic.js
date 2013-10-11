joint.shapes.tonic = {};

joint.shapes.tonic.Class = joint.shapes.basic.Generic.extend({

    markup: [
        '<g class="rotatable">',
          '<g class="scalable">',
            '<rect class="tonic-class-name-rect"/><rect class="tonic-class-attrs-rect"/><rect class="tonic-class-methods-rect"/>',
          '</g>',
          '<text class="tonic-class-name-text"/><text class="tonic-class-attrs-text"/><text class="tonic-class-methods-text"/>',
        '</g>'
    ].join(''),

    defaults: joint.util.deepSupplement({

        type: 'tonic.Class',

        attrs: {
            rect: { 'width': 200, 'stroke': 'black', 'stroke-width': 2, 'fill': '#2980b9' },
            text: { 'fill': 'black', 'font-size': 12, 'font-family': 'Times New Roman' },

            '.tonic-class-name-rect': { 'fill': '#3498db' },
            '.tonic-class-attrs-rect': {},
            '.tonic-class-methods-rect': {},

            '.tonic-class-name-text': { 'ref': '.tonic-class-name-rect', 'ref-y': .5, 'ref-x': .5, 'text-anchor': 'middle', 'y-alignment': 'middle', 'font-weight': 'bold' },
            '.tonic-class-attrs-text': { 'ref': '.tonic-class-attrs-rect', 'ref-y': 5, 'ref-x': 5 },
            '.tonic-class-methods-text': { 'ref': '.tonic-class-methods-rect', 'ref-y': 5, 'ref-x': 5 }

        },

        name: [],
        attributes: [],
        methods: []

    }, joint.shapes.basic.Generic.prototype.defaults),

    initialize: function() {

        _.bindAll(this, 'updateRectangles');

        this.on('change:name change:attributes change:methods', function() {
            this.updateRectangles();
            this.trigger('change:attrs');
        });

        this.updateRectangles();

        joint.shapes.basic.Generic.prototype.initialize.apply(this, arguments);
    },

    getClassName: function() {
        return this.get('name');
    },

    updateRectangles: function() {

        var attrs = this.get('attrs');

        var rects = [
            { type: 'name', text: this.getClassName() },
            { type: 'attrs', text: this.get('attributes') },
            { type: 'methods', text: this.get('methods') }
        ];

        var offsetY = 0;

        _.each(rects, function(rect) {

            var lines = _.isArray(rect.text) ? rect.text : [rect.text];
	    var rectHeight = lines.length * 20 + 20;

            attrs['.tonic-class-' + rect.type + '-text'].text = lines.join('\n');
            attrs['.tonic-class-' + rect.type + '-rect'].height = rectHeight;
            attrs['.tonic-class-' + rect.type + '-rect'].transform = 'translate(0,'+ offsetY + ')';

            offsetY += rectHeight;
        });
    }

});

joint.shapes.tonic.Abstract = joint.shapes.tonic.Class.extend({

    defaults: joint.util.deepSupplement({
        type: 'tonic.Abstract',
        attrs: { 'rect' : { fill : '#c0392b' }, '.tonic-class-name-rect': { fill : '#e74c3c' }}
    }, joint.shapes.tonic.Class.prototype.defaults),

    getClassName: function() {
        return ['<<Abstract>>', this.get('name')];
    }

});

joint.shapes.tonic.Interface = joint.shapes.tonic.Class.extend({

    defaults: joint.util.deepSupplement({
        type: 'tonic.Interface',
        attrs: { 'rect' : { fill : '#f39c12' }, '.tonic-class-name-rect': { fill : '#f1c40f' }}
    }, joint.shapes.tonic.Class.prototype.defaults),

    getClassName: function() {
        return ['<<Interface>>', this.get('name')];
    }

});

joint.shapes.tonic.Generalization = joint.dia.Link.extend({
    defaults: {
        type: 'tonic.Generalization',
        attrs: { '.marker-target': { d: 'M 20 0 L 0 10 L 20 20 z', fill: 'white' }}
    }
});

joint.shapes.tonic.Implementation = joint.dia.Link.extend({
    defaults: {
        type: 'tonic.Implementation',
        attrs: {
            '.marker-target': { d: 'M 20 0 L 0 10 L 20 20 z', fill: 'white' },
            '.connection': { 'stroke-dasharray': '3,3' }
        }
    }
});

joint.shapes.tonic.Aggregation = joint.dia.Link.extend({
    defaults: {
        type: 'tonic.Aggregation',
        attrs: { '.marker-target': { d: 'M 40 10 L 20 20 L 0 10 L 20 0 z', fill: 'white' }}
    }
});

joint.shapes.tonic.Composition = joint.dia.Link.extend({
    defaults: {
        type: 'tonic.Composition',
        attrs: { '.marker-target': { d: 'M 40 10 L 20 20 L 0 10 L 20 0 z', fill: 'black' }}
    }
});

joint.shapes.tonic.Association = joint.dia.Link.extend({
    defaults: { type: 'tonic.Association' }
});

joint.shapes.tonic.TaskApp = joint.shapes.basic.Generic.extend({

    markup: [
        '<g class="rotatable">',
          '<g class="scalable">',
            '<rect/>',
          '</g>',
          '<path/><text class="tonic-taskApp-name"/><text class="tonic-taskApp-events"/>',
        '</g>'
    ].join(''),

    defaults: joint.util.deepSupplement({

        type: 'tonic.State',

        attrs: {
            rect: { 'width': 200, 'height': 200, 'fill': '#ffffff', 'stroke': '#000000', 'stroke-width': 3, 'rx': 10, 'ry': 10 },
            path: { 'd': 'M 0 20 L 200 20', 'stroke': '#000000', 'stroke-width': 2 },
            text: { 'font-family': 'Tahoma', 'font-size': 14, fill: 'black' },
            '.tonic-taskApp-name': { 'ref': 'rect', 'ref-x': .5, 'ref-y': 5, 'text-anchor': 'middle', 'font-weight': 'bold'},
            '.tonic-taskApp-events': { 'ref': 'path', 'ref-x': 5, 'ref-y': 5 }
        },

        name: 'State',
        events: []

    }, joint.shapes.basic.Generic.prototype.defaults),

    initialize: function() {

        _.bindAll(this, 'updateEvents', 'updatePath');

        this.on({
            'change:name': function() { this.updateName(); this.trigger('change:attrs'); },
            'change:events': function() { this.updateEvents(); this.trigger('change:attrs'); },
            'change:size': this.updatePath
        });

        this.updateName();
        this.updateEvents();
        this.updatePath();

        joint.shapes.basic.Generic.prototype.initialize.apply(this, arguments);
    },

    updateName: function() {
        this.get('attrs')['.tonic-taskApp-name'].text = this.get('name');
    },

    updateEvents: function() {
        this.get('attrs')['.tonic-taskApp-events'].text = this.get('events').join('\n');
    },

    updatePath: function() {
        this.get('attrs')['path'].d = 'M 0 20 L ' + this.get('size').width + ' 20';
    }

});

joint.shapes.tonic.StartState = joint.shapes.basic.Generic.extend({
    markup: '<g class="rotatable"><g class="scalable"><polygon points="0,0 24,12 0,24" class="outer"/><polygon points="0,0 24,12 0,24" class="inner"/></g></g>',

    defaults: joint.util.deepSupplement({

        type: 'tonic.StartState',
        size: { width: 20, height: 20 },
        attrs: {
            'polygon.outer': {
                transform: 'translate(10, 10)',
                r: 10,
                fill: '#000000',
                stroke: '#000000'
            },

            'polygon.inner': {
                transform: 'translate(10, 10)',
                r: 6,
                fill: '#000000'
            }
        }

    }, joint.shapes.basic.Generic.prototype.defaults)
});

joint.shapes.tonic.StopState = joint.shapes.basic.Rect.extend({
    defaults: joint.util.deepSupplement({

        type: 'tonic.StopState',
        size: { width: 20, height: 20 },
        attrs: {
          rect: { 'width': 200, 'height': 200, 'fill': '#000000', 'stroke': '#000000', 'stroke-width': 0, 'rx': 10, 'ry': 10 },
        }

    }, joint.shapes.basic.Rect.prototype.defaults)
});

joint.shapes.tonic.Return = joint.shapes.basic.Circle.extend({
    defaults: joint.util.deepSupplement({
        type: 'tonic.Return',
        attrs: {
            circle: { 'stroke-width': 3 },
            text: { 'font-weight': 'bold' }
        }
    }, joint.shapes.basic.Circle.prototype.defaults)
});

joint.shapes.tonic.Bind = joint.dia.Link.extend({
    defaults: {
        type: 'tonic.Bind',
        attrs: {
            '.marker-target': { d: 'M 10 0 L 0 5 L 10 10 z', fill: '#000000', stroke: '#000000' },
            '.connection': { stroke: '#000000' }
        }
    }
});

