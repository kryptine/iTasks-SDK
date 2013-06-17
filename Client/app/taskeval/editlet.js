function __iTasks_Framework_ClientInterface_getDomElement(id, w){
    w = Sapl.feval(w);
    id = Sapl.feval(id);
    
    return ___predefined__Tuple2(w.document.getElementById(id), w);
}

function __iTasks_Framework_ClientInterface_getObjectAttr(e, attr, w){
    w = Sapl.feval(w);
    e = Sapl.feval(e);
    attr = Sapl.feval(attr);    
    
    var value = eval("e."+attr+";");
    
    return ___predefined__Tuple3(e, value, w);
}

function __iTasks_Framework_ClientInterface_runObjectMethod(obj, method, params, w){
    w = Sapl.feval(w);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);

	var eargs = [obj, method];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);
    return ___predefined__Tuple3(obj, value, w);
}

function __iTasks_Framework_ClientInterface_setObjectAttr(e, attr, value, w){
    w = Sapl.feval(w);
    e = Sapl.feval(e);
    value = Sapl.toJS(Sapl.feval(value));
    attr = Sapl.feval(attr);      
    
	// unbox function value, boxed by Sapl.feval
	if(isArray(value) && value.length == 2 && typeof (value[0]) == "function" && value[1].length == 0){
		value = value[0];
	}
	
    eval("e."+attr+"=value;");
    return ___predefined__Tuple3(e, value, w);
}

function __iTasks_Framework_ClientInterface_getDomAttr(id, attr, w){
    w = Sapl.feval(w);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);

    var value = eval("w.document.getElementById(\""+id+"\")."+attr+";");
    return ___predefined__Tuple2(value, w);
}

function __iTasks_Framework_ClientInterface_setDomAttr(id, attr, value, w){
    w = Sapl.feval(w);
    value = Sapl.feval(value);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);
    
    eval("w.document.getElementById(\""+id+"\")."+attr+"=value;");
    return ___predefined__Tuple2(value, w);
}

function __iTasks_Framework_ClientInterface_findObject(name, w){
    w = Sapl.feval(w);
    name = Sapl.feval(name);

    var obj;
	try{
		eval("obj = "+name+";");
	} catch (e) {	
		// possibly undefined. don't do anything. 
	}
	
    return ___predefined__Tuple2(obj, w);
}

function __iTasks_Framework_ClientInterface_createObject(obj, params, w){
    w = Sapl.feval(w);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);

	var eargs = [obj, null];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);	
    return ___predefined__Tuple2(value, w);
}

function __iTasks_Framework_ClientInterface_loadExternalJS(url, continuation, w){
    w = Sapl.feval(w);
    continuation = Sapl.feval(continuation);
    url = Sapl.feval(url);	
		
    var script=document.createElement('script');
    script.setAttribute("type","text/javascript");
    script.onload = continuation;
	
    script.setAttribute("src", url);
    document.getElementsByTagName("head")[0].appendChild(script);
	
    return w;
}

function __iTasks_Framework_ClientInterface_isUndefined(obj, win){
    win = Sapl.feval(win);
    obj = Sapl.feval(obj);
	
    return ___predefined__Tuple2(obj == null, win);
}

function __iTasks_Framework_ClientInterface_toHtmlObject(val, win){
  win = Sapl.feval(win);
  val = Sapl.feval(val);
  return ___predefined__Tuple2(Sapl.toJS(val), win);
}

function __iTasks_Framework_ClientInterface_fromHtmlObject(obj, win){
  return ___predefined__Tuple2(Sapl.feval(obj), win);
}

