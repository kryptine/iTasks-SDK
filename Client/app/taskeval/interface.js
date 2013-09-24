//Small versions

function ___wrapJS(val){
	return [0,"JSVal",val];
}

function ___unwrapJS(ptr){
	return ptr[2];
}

//jsNull :: (JSVal a)
function __iTasks_API_Core_Client_Interface_jsNull() {
	return ___wrapJS(null);
}
//jsWindow :: (JSVal JSWindow)
function __iTasks_API_Core_Client_Interface_jsWindow() {
	return ___wrapJS(window);
}

//jsDocument :: (JSVal JSDocument)
function __iTasks_API_Core_Client_Interface_jsDocument() {
	return ___wrapJS(document);
}

//jsThis :: !*JSWorld -> (!JSVal a,!*JSWorld)
function __iTasks_API_Core_Client_Interface_jsThis(world) {
	world = Sapl.feval(world);
	return ___predefined__Tuple2(___wrapJS(this), world);
}

//jsEmptyObject :: !*JSWorld -> (!JSVal a, !*JSWorld)
function __iTasks_API_Core_Client_Interface_jsEmptyObject(world) {
	world = Sapl.feval(world);
	return ___predefined__Tuple2(___wrapJS({}), world);
}

//jsNewObject :: !String ![JSPtr a] !*JSWorld -> *(!JSPtr b, !*JSWorld)
function __iTasks_API_Core_Client_Interface_jsNewObject(cons_name, args, world){
    world = Sapl.feval(world);
	cons_name = Sapl.feval(cons_name);
	args = Sapl.toJS(Sapl.feval(args)); 
	
	var args = [null].concat(args);
    var factoryFunction = constructor.bind.apply(eval(cons_name), args);
    return ___predefined__Tuple2(___wrapJS(new factoryFunction()), world);
}

//jsGetObjectAttr :: !String !(JSVal a) !*JSWorld -> *(!JSVal b, !*JSWorld)
function __iTasks_API_Core_Client_Interface_jsGetObjectAttr(attr,obj,world) {
	
	world = Sapl.feval(world);	
	attr = Sapl.feval(attr);
	obj = ___unwrapJS(Sapl.feval(obj));

	var value;

	try{
		value = eval("obj."+attr+";");
	}catch(err){
		value = undefined;
	}
	
	return ___predefined__Tuple2(___wrapJS(value), world);
}

//jsGetObjectEl :: !Int !(JSVal o) !*JSWorld -> *(!JSVal b, !*JSWorld)
function __iTasks_API_Core_Client_Interface_jsGetObjectEl(index,obj,world) {

	world = Sapl.feval(world);
	index = Sapl.feval(index);
	obj = ___unwrapJS(Sapl.feval(obj));

	return ___predefined__Tuple2(___wrapJS(obj[index]), world);
}

//jsSetObjectAttr :: !String !(JSVal v) !(JSVal o) !*JSWorld -> *JSWorld
function __iTasks_API_Core_Client_Interface_jsSetObjectAttr(attr,value,obj,world) {
    
	world = Sapl.feval(world);
	attr = Sapl.feval(attr);   
    value = ___unwrapJS(Sapl.feval(value));
    obj = ___unwrapJS(Sapl.feval(obj));

    eval("obj."+attr+"=value;");
    
    return ___predefined__Tuple2(___wrapJS(obj), world);
}

//jsSetObjectEl :: !Int !(JSVal v) !(JSVal o) !*JSWorld -> *JSWorld
function __iTasks_API_Core_Client_Interface_jsSetObjectEl(index,value,obj,world) {

    world = Sapl.feval(world);
    index = Sapl.feval(index);   
    value = ___unwrapJS(Sapl.feval(value));
    obj = ___unwrapJS(Sapl.feval(obj));
    	
    obj[index] = value;
    
    return ___predefined__Tuple2(___wrapJS(obj), world);
}

//jsApply :: !(JSVal (JSFunction f)) !(JSVal scope) [JSVal args] !*JSWorld -> *(!JSVal a, !*JSWorld)
function __iTasks_API_Core_Client_Interface_jsApply(fun,scope,args,world) {
	world = Sapl.feval(world);
	fun = ___unwrapJS(Sapl.feval(fun));
	scope = ___unwrapJS(Sapl.feval(scope));
	args = Sapl.toJS(Sapl.feval(args)); 
	
	return ___predefined__Tuple2(___wrapJS(fun.apply(scope,args)), world);
}

//jsTypeof :: !(JSVal a) -> !String
function __iTasks_API_Core_Client_Interface_jsTypeof(obj) {
	obj = ___unwrapJS(Sapl.feval(obj));
	return (typeof obj);
}

// newJSArray :: !*JSWorld -> *(!JSVal [a], !*JSWorld)
function __iTasks_API_Core_Client_Interface_newJSArray(world){
	world = Sapl.feval(world);
	return ___predefined__Tuple2(___wrapJS([]), world);
}

// toJSVal :: !a -> JSVal b
function __iTasks_API_Core_Client_Interface_toJSVal(val){

	val = Sapl.feval(val);

	var eventHandler = function(expr){
		
		var h = function(){
			expr[1].concat(arguments);
			Sapl.feval(expr);
		};
		
		return h;
    }

	// if an argument is a function, we handle it as an event handler
	if(isArray(val) && isFunction(val[0])){
		return ___wrapJS(eventHandler(val));
	}else{
		return ___wrapJS(Sapl.toJS(val));
	}
}

// toJSArg :: !a -> JSArg
function __iTasks_API_Core_Client_Interface_toJSArg(val){

	val = Sapl.feval(val);
	
	if(isArray(val) && val[1]=="JSVal"){
		return val;
	}else{
		return __iTasks_API_Core_Client_Interface_toJSVal(val);
	}
}		
	
// fromJSVal :: !(JSVal a) -> Dynamic
function __iTasks_API_Core_Client_Interface_fromJSVal(ptr){
	return toDynamic(___unwrapJS(Sapl.feval(ptr)));
}

// createTaskletEventHandler :: (HtmlEventHandlerFunc a e) !TaskId -> (JSVal b) 
function __iTasks_API_Core_Client_Tasklet_createTaskletEventHandler(expr, taskId){
	expr = Sapl.feval(expr);
	taskId = Sapl.feval(taskId);
	
    // Creating a closure of 2. layer
    var eventHandler = function(expr, taskId){
		
		var h = function(event){
			return __iTasks_Framework_Client_Tasklet_handleJSEvent(expr, taskId, event);
		};
		
		return h;
    }
	
	return ___wrapJS(eventHandler(expr, taskId));
}

// createEditletEventHandler :: (ComponentEventHandlerFunc a st) !ComponentId -> (JSVal (JSFunction b)) 
function __iTasks_API_Core_Client_Editlet_createEditletEventHandler(expr, componentId){
	
	var comp = itwc.global.controller[componentId];
	return ___wrapJS(comp.eventHandler(expr));
}
