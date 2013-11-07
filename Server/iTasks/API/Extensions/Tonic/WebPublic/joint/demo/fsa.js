var graph = new joint.dia.Graph;

var paper = new joint.dia.Paper({
    el: $('#paper'),
    width: 300,
    height: 300,
    gridSize: 1,
    model: graph
});

var start = new joint.shapes.fsa.StartState({ position: { x: 100, y: 100 } });
graph.addCell(start);
