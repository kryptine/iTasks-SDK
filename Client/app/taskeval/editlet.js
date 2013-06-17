function __ClientInterface_getDomElement(id, d){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    
    return ___predefined__Tuple2(d.getElementById(id), d);
}

function __ClientInterface_getObjectAttr(e, attr, d){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    attr = Sapl.feval(attr);    
    
    var value = eval("e."+attr+";");
    
    return ___predefined__Tuple3(e, value, d);
}

function __ClientInterface_runObjectMethod(obj, method, params, d){
    d = Sapl.feval(d);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);

	var eargs = [obj, method];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);
    return ___predefined__Tuple3(obj, value, d);
}

function __ClientInterface_setObjectAttr(e, attr, value, d){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    value = Sapl.toJS(Sapl.feval(value));
    attr = Sapl.feval(attr);      
    
	// unbox function value, boxed by Sapl.feval
	if(isArray(value) && value.length == 2 && typeof (value[0]) == "function" && value[1].length == 0){
		value = value[0];
	}
	
    eval("e."+attr+"=value;");
    return ___predefined__Tuple3(e, value, d);
}

function __ClientInterface_getDomAttr(id, attr, d){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);

    var value = eval("d.getElementById(\""+id+"\")."+attr+";");
    return ___predefined__Tuple2(value, d);
}

function __ClientInterface_setDomAttr(id, attr, value, d){
    d = Sapl.feval(d);
    value = Sapl.feval(value);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);
    
    eval("d.getElementById(\""+id+"\")."+attr+"=value;");
    return ___predefined__Tuple2(value, d);
}

function __ClientInterface_findObject(name, d){
    d = Sapl.feval(d);
    name = Sapl.feval(name);

    var obj;
	try{
		eval("obj = "+name+";");
	} catch (e) {	
		// possibly undefined. don't do anything. 
	}
	
    return ___predefined__Tuple2(obj, d);
}

function __ClientInterface_createObject(obj, params, d){
    d = Sapl.feval(d);
    params = Sapl.toJS(Sapl.feval(params));
    obj = Sapl.feval(obj);

	var eargs = [obj, null];
	for(var i=0; i<params.length; i++){
		eargs.push(params[i]);
	}
	
    var value = streval.apply(null, eargs);	
    return ___predefined__Tuple2(value, d);
}

function __ClientInterface_loadExternalJS(url, continuation, d){
    d = Sapl.feval(d);
    continuation = Sapl.feval(continuation);
    url = Sapl.feval(url);	
		
    var script=document.createElement('script');
    script.setAttribute("type","text/javascript");
    script.onload = continuation;
	
    script.setAttribute("src", url);
    document.getElementsByTagName("head")[0].appendChild(script);
	
    return d;
}

function __ClientInterface_isUndefined(obj, win){
    win = Sapl.feval(win);
    obj = Sapl.feval(obj);
	
    return ___predefined__Tuple2(obj == null, win);
}

function __ClientInterface_toHtmlObject(val, win){
  win = Sapl.feval(win);
  val = Sapl.feval(val);
  return ___predefined__Tuple2(Sapl.toJS(val), win);
}

function __ClientInterface_fromHtmlObject(obj, win){
  return ___predefined__Tuple2(Sapl.feval(obj), win);
}

