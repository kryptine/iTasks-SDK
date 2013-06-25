//Small versions


//jsNull :: (JSPtr a)
function __iTasks_Framework_ClientInterface_jsNull() {
	return null;
}
//jsWindow :: (JSPtr JSWindow)
function __iTasks_Framework_ClientInterface_jsWindow() {
	return window;
}
//jsThis :: !*JSWorld -> (!JSPtr a,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsThis(world) {
	world = Sapl.feval(world);
	return ___predefined__Tuple2([this], [world]);
}
//jsEmptyObject :: !*JSWorld -> (!JSPtr a, !*JSWorld)
function __iTasks_Framework_ClientInterface_jsEmptyObject(world) {
	world = Sapl.feval(world);
	return ___predefined__Tuple2([{}], [world]);
}
//jsNewObject	:: !(JSPtr JSFunction) !*JSWorld -> (!JSPtr a, !*JSWorld)
function __iTasks_Framework_ClientInterface_jsNewObject(cons,world) {
	cons = Sapl.feval(cons);
	world = Sapl.feval(world);
	return ___predefined__Tuple2([new cons], [world]);
}

//jsGetObjectAttr :: !String !(JSPtr a) !*JSWorld -> (!b,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsGetObjectAttr(attr,obj,world) {
	
	attr = Sapl.feval(attr);
	obj = Sapl.feval(obj);
	world = Sapl.feval(world);

	return ___predefined__Tuple2([obj[attr]], [world]);
}
//jsGetObjectEl :: !Int !(JSPtr a) !*JSWorld -> (!b,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsGetObjectEl(index,obj,world) {
	
	index = Sapl.feval(index);
	obj = Sapl.feval(obj);
	world = Sapl.feval(world);

	return ___predefined__Tuple2([obj[index]], [world]);
}
//jsSetObjectAttr :: !String !b !(JSPtr a) !*JSWorld -> *JSWorld
function __iTasks_Framework_ClientInterface_jsSetObjectAttr(attr,value,obj,world) {

    attr = Sapl.feval(attr);   
    value = Sapl.feval(value);
    obj = Sapl.feval(obj);
    world = Sapl.feval(world);
    
	// unbox function value, boxed by Sapl.feval
	if(isArray(value) && value.length == 2 && typeof (value[0]) == "function" && value[1].length == 0){
		value = value[0];
	}
	
    obj[attr] = value;
    
    return [world];
}
//jsSetObjectEl :: !Int !b !(JSPtr a) !*JSWorld -> *JSWorld
function __iTasks_Framework_ClientInterface_jsSetObjectEl(index,value,obj,world) {

    index = Sapl.feval(index);   
    value = Sapl.feval(value);
    obj = Sapl.feval(obj);
    world = Sapl.feval(world);
    
	// unbox function value, boxed by Sapl.feval
	if(isArray(value) && value.length == 2 && typeof (value[0]) == "function" && value[1].length == 0){
		value = value[0];
	}
	
    obj[index] = value;
    
    return [world];
}
//jsApply :: !(JSPtr JSFunction) !(JSPtr a) !(JSPtr b) !*JSWorld -> (!c,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsApply(fun,scope,args,world) {
	fun = Sapl.feval(fun);
	scope = Sapl.feval(scope);
	args = Sapl.feval(args);
	world = Sapl.feval(world);
	
	return ___predefined__Tuple2([fun.apply(scope,args)], [world]);
}

//jsTypeof :: !a !*JSWorld -> (!String,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsTypeof(obj,world) {
	obj = Sapl.feval(obj);
	world = Sapl.feval(world);

	return ___predefined__Tuple2([typeof obj],[world])
}

//jsCallObjectMethod :: !String ![b] !(JSPtr a)  !*JSWorld -> (!c,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsCallObjectMethod(attr,args,obj,world) {
	
	attr = Sapl.feval(attr);
    args = Sapl.toJS(Sapl.feval(args)); //TODO, check if just converting the spine is enough
    obj = Sapl.feval(obj);
	world = Sapl.feval(world);

	console.log(obj,attr,args);
    return ___predefined__Tuple2([(obj[attr]).apply(obj,args)], [world]);
}

//jsWrapFun :: !a !*JSWorld -> (!JSPtr JSFunction,!*JSWorld)
function __iTasks_Framework_ClientInterface_jsWrapFun(fun,world) {
	fun = Sapl.feval(fun);
	world = Sapl.feval(world);
		
	var jsfun = function() {
		//Prepare sapl expression
		var args = [];
		for(var i = 0; i < arguments.length; i++) {
			args.push([arguments[i]]);
		}
		args.push(["JSWorld"]);
		
		var res = Sapl.toJS(Sapl.feval([fun,args]));
		
		if(res.length > 1) {
			return res[0];
		}
	};
	
	return ___predefined__Tuple2([jsfun], [world]);
}
