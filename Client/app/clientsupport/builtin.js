// TO BE SURE
function ___predefined__Tuple3(__a1, __a2, __a3) {
	return [0, '_predefined._Tuple3', __a1, __a2, __a3];
};

function ___predefined__Tuple2(__a1, __a2) {
	return [0, '_predefined._Tuple2', __a1, __a2];
};
// TO BE SURE

function __atan(a){
    return Math.atan(Sapl.feval(a));
}

function __mult(a,b){
    return Sapl.feval(a)*Sapl.feval(b);
}

function __div(a,b){
    return Math.floor(Sapl.feval(a)/Sapl.feval(b));
}

function __divreal(a,b){
    return Sapl.feval(a)/Sapl.feval(b);
}

function __add(a,b){
    return Sapl.feval(a)+Sapl.feval(b);
}

function __sub(a,b){
    return Sapl.feval(a)-Sapl.feval(b);
}

function __eq(a,b){
    return Sapl.feval(a)==Sapl.feval(b);
}	

function __neq(a,b){
    return Sapl.feval(a)!=Sapl.feval(b);
}	

function __mod(a,b){
    return Sapl.feval(a)%Sapl.feval(b);
}	

function __gt(a,b){
    return Sapl.feval(a)>Sapl.feval(b);
}	

function __ge(a,b){
    return Sapl.feval(a)>=Sapl.feval(b);
}

function __not(a){
    return !Sapl.feval(a);
}

function __error(str){
    throw "ERROR: "+str;
}

function __abort(str){
    throw "ABORT: "+str;
}

function __sapldebug(str, f){
    if(confirm(str)){
        return f;
    }else{
        _error();
    }
}

function __strlen(str){
    return Sapl.feval(str).length;
}

function __string_select(str, pos){
    return Sapl.feval(str).charAt(Sapl.feval(pos));
}  

function ___string_create(length){
    var nl = Sapl.feval(length);
    var r=""; for (var a=0;a<nl;a++) r+="\0"; 
    return r;
}

function __string_create(length, c){
    var nl = Sapl.feval(length);
    var nc = Sapl.feval(c);
    var r=""; for (var a=0;a<nl;a++) r+=nc; 
    return r;
}

function __string_update(str, index, c){
    var nstr = Sapl.feval(str);
    var nc = Sapl.feval(c);
    var nindex = Sapl.feval(index);
    return nstr.substr(0, nindex) + nc + nstr.substr(nindex+nc.length);
}

function __string_slice(str, ind1, ind2){
    var nstr = Sapl.feval(str);
    var nind1 = Sapl.feval(ind1);
    var nind2 = Sapl.feval(ind2);
    
    return nstr.substr(nind1, nind2-nind1+1);
}

function __string_append(str1, str2){
    var nstr1 = Sapl.feval(str1);
    var nstr2 = Sapl.feval(str2);
    return nstr1+nstr2;
}

function __toInt_char(chr){
    var nchr = Sapl.feval(chr);
    return nchr.charCodeAt(0);
}

function __toInt_str(str){
    var nstr = Sapl.feval(str);
    return parseInt(nstr);
}

function __toInt_real(r){
    var nr = Sapl.feval(r);
    return Math.floor(nr);
}

function __toReal(str){
    var nstr = Sapl.feval(str);
    return parseFloat(nstr);
}

function __sqrt(num){
    return Math.sqrt(Sapl.feval(num));
}

function __toChar(code){
    var ncode = Sapl.feval(code);
    return String.fromCharCode(ncode);
}

function __bitand(a,b){
    return Sapl.feval(a) & Sapl.feval(b);
}

function __shiftleft(a,b){
    return Sapl.feval(a) << Sapl.feval(b);
}

function __shiftright(a,b){
    return Sapl.feval(a) >>> Sapl.feval(b);
}

function __SaplHtml_getDomElement(d, id){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    
    return ___predefined__Tuple2(d, d.getElementById(id));
}

function __SaplHtml_getTime(w){
	var d = new Date();
	return ___predefined__Tuple2(d.getTime(), w);
}

function __SaplHtml_createWorld(){
	return 123456;
}

function __SaplHtml_getObjectAttr(d, e, attr){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    attr = Sapl.feval(attr);    
    
    var value = eval("e."+attr+";");

    if(isNumber(value)){
        value = value.toString();
    }else if(isBoolean(value)){
        value = value.toString();
    }
    
    return ___predefined__Tuple3(d, e, value);
}

function __SaplHtml_getObjectAttrObject(d, e, attr){
    return __SaplHtml_getObjectAttr(d, e, attr);
}

function __SaplHtml_runObjectMethod0(d, obj, method){
    d = Sapl.feval(d);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);

    var value = streval(obj, method);
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_runObjectMethod1(d, obj, method, param){
    d = Sapl.feval(d);
    param = Sapl.feval(param);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);

    var value = streval(obj, method, param);
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_runObjectMethod2(d, obj, method, p1, p2){
    d = Sapl.feval(d);
    p1 = Sapl.feval(p1);
    p2 = Sapl.feval(p2);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);
    
    var value = streval(obj, method, p1, p2);    
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_runObjectMethod3(d, obj, method, p1, p2, p3){
    d = Sapl.feval(d);
    p1 = Sapl.feval(p1);
    p2 = Sapl.feval(p2);
    p3 = Sapl.feval(p3);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);
    
    var value = streval(obj, method, p1, p2, p3);    
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_runObjectMethod4(d, obj, method, p1, p2, p3, p4){
    d = Sapl.feval(d);
    p1 = Sapl.feval(p1);
    p2 = Sapl.feval(p2);
    p3 = Sapl.feval(p3);
    p4 = Sapl.feval(p4);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);
    
    var value = streval(obj, method, p1, p2, p3, p4);    
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_runObjectMethod6(d, obj, method, p1, p2, p3, p4, p5, p6){
    d = Sapl.feval(d);
    p1 = Sapl.feval(p1);
    p2 = Sapl.feval(p2);
    p3 = Sapl.feval(p3);
    p4 = Sapl.feval(p4);
    p5 = Sapl.feval(p5);
    p6 = Sapl.feval(p6);
    obj = Sapl.feval(obj);
    method = Sapl.feval(method);
    
    var value = streval(obj, method, p1, p2, p3, p4, p5, p6);    
    return ___predefined__Tuple3(d, obj, value);
}

function __SaplHtml_setObjectAttr(d, e, attr, value){
    d = Sapl.feval(d);
    e = Sapl.feval(e);
    value = Sapl.feval(value);
    attr = Sapl.feval(attr);      
    
    eval("e."+attr+"=value;");
    return ___predefined__Tuple3(d, e, value);
}

function __SaplHtml_getDomAttr(d, id, attr){
    d = Sapl.feval(d);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);

    var value = eval("d.getElementById(\""+id+"\")."+attr+";");
    return ___predefined__Tuple2(d, value);
}

function __SaplHtml_setDomAttr(d, id, attr, value){
    d = Sapl.feval(d);
    value = Sapl.feval(value);
    id = Sapl.feval(id);
    attr = Sapl.feval(attr);
    
    eval("d.getElementById(\""+id+"\")."+attr+"=value;");
    return ___predefined__Tuple2(d, value);
}

function __SaplHtml_findObject(d, name){

    d = Sapl.feval(d);
    name = Sapl.feval(name);

    var obj;
    eval("obj = "+name+";");
	
    return ___predefined__Tuple2(d, obj);
}

function __SaplHtmlClientSupport_onClient(){
	return true;
}

function __sapldebug_sapldebug(a,b){
	console.log("DEBUG: "+Sapl.toString(a));
	return b;
}

function __dynamic_string_copy_to_string(a){
	return Sapl.dynamicToString(Sapl.feval(a));
}

function __dynamic_string_copy_from_string(a){
	eval("var tmp="+Sapl.feval(a)+";");
	return ___predefined__Tuple2(tmp, a); // TODO: second?
}

function __Base64_base64Encode(a){
	return window.btoa(Sapl.feval(a));
}

function __Base64_base64Decode(a){
	return window.atob(Sapl.feval(a));
}
