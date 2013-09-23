// TO BE SURE
function ___predefined__Tuple3(__a1, __a2, __a3) {
	return [0, '_predefined._Tuple3', __a1, __a2, __a3];
};

function ___predefined__Tuple2(__a1, __a2) {
	return [0, '_predefined._Tuple2', __a1, __a2];
};
// TO BE SURE

// --------- Primitive functions -----------------------------

function print(val){
	val = Sapl.toJS(Sapl.feval(val));
	var out = document.getElementById("output");
	if(out){
		if(val == "\n"){
			var br = document.createElement("BR");
			out.appendChild(br);
		}else{
			var span = document.createElement("SPAN");
			span.innerHTML = val;
			out.appendChild(span);
		}
	}else{
		console.log(val);
	}
}

function println(val){
	print(val);
	print("\n");
}

function __trace(val, cont){
	print(val);
	return Sapl.feval(cont);
}

function __traceln(val, cont){
	println(val);
	return Sapl.feval(cont);
}

// --------- Primitive functions -----------------------------

function _atan(a){
    return Math.atan(Sapl.feval(a));
}

function _acos(a){
    return Math.acos(Sapl.feval(a));
}

function _mult(a,b){
    return Sapl.feval(a)*Sapl.feval(b);
}

function _div(a,b){
    return Math.floor(Sapl.feval(a)/Sapl.feval(b));
}

function _divreal(a,b){
    return Sapl.feval(a)/Sapl.feval(b);
}

function _add(a,b){
    return Sapl.feval(a)+Sapl.feval(b);
}

function _sub(a,b){
    return Sapl.feval(a)-Sapl.feval(b);
}

function _eq(a,b){
    return Sapl.feval(a)==Sapl.feval(b);
}	

function _neq(a,b){
    return Sapl.feval(a)!=Sapl.feval(b);
}	

function _pow(a,b){
    return Math.pow(Sapl.feval(a),Sapl.feval(b));
}	

function _sin(a){
    return Math.sin(Sapl.feval(a));
}	

function _cos(a){
    return Math.cos(Sapl.feval(a));
}	

function _mod(a,b){
    return Sapl.feval(a)%Sapl.feval(b);
}	

function _gt(a,b){
    return Sapl.feval(a)>Sapl.feval(b);
}	

function _ge(a,b){
    return Sapl.feval(a)>=Sapl.feval(b);
}

function _lt(a,b){
    return Sapl.feval(a)<Sapl.feval(b);
}	

function _not(a){
    return !Sapl.feval(a);
}

function _and(a,b){
    return Sapl.feval(a) && Sapl.feval(b);
}

function _or(a,b){
    return Sapl.feval(a) || Sapl.feval(b);
}

function _abs(a){
    return Math.abs(Sapl.feval(a));
}

function _neg(a){
    return Sapl.feval(a) * -1;
}

function _error(str){
    throw "ERROR: "+str;
}

function _abort(str){
    throw "ABORT: "+str;
}

function __dynamic_handler(){
	return "DYNVAL";
}

function __sapldebug(str, f){
    if(confirm(str)){
        return f;
    }else{
        _error();
    }
}

function _strlen(str){
    return Sapl.feval(str).length;
}

function _string_select(str, pos){
	var str = Sapl.feval(str);
	return str.charAt(Sapl.feval(pos));
}  

function _string_create1(length){
    var nl = Sapl.feval(length);
    var r=""; for (var a=0;a<nl;a++) r+="\0"; 
    return r;
}

function _string_create2(length, c){
    var nl = Sapl.feval(length);
    var nc = Sapl.feval(c);
    var r=""; for (var a=0;a<nl;a++) r+=nc; 
    return r;
}

function _string_update(str, index, c){
    var nstr = Sapl.feval(str);
    var nc = Sapl.feval(c);
    var nindex = Sapl.feval(index);
    return nstr.substr(0, nindex) + nc + nstr.substr(nindex+nc.length);
}

function _string_slice(str, ind1, ind2){
    var nstr = Sapl.feval(str);
    var nind1 = Sapl.feval(ind1);
    var nind2 = Sapl.feval(ind2);
    
    return nstr.substr(nind1, nind2-nind1+1);
}

function _string_append(str1, str2){
    var nstr1 = Sapl.feval(str1);
    var nstr2 = Sapl.feval(str2);
    return nstr1+nstr2;
}

function _toInt_char(chr){
    var nchr = Sapl.feval(chr);
    return nchr.charCodeAt(0);
}

function _toInt_str(str){
    var nstr = Sapl.feval(str);
    return parseInt(nstr);
}

function _toInt_real(r){
    var nr = Sapl.feval(r);
    return Math.floor(nr);
}

function _toReal(str){
    var nstr = Sapl.feval(str);
    return parseFloat(nstr);
}

function _sqrt(num){
    return Math.sqrt(Sapl.feval(num));
}

function _toChar(code){
    var ncode = Sapl.feval(code);
    return String.fromCharCode(ncode);
}

function _toString(a){
    var na = Sapl.feval(a);
    return na + "";
}

function _bitand(a,b){
    return Sapl.feval(a) & Sapl.feval(b);
}

function _shiftleft(a,b){
    return Sapl.feval(a) << Sapl.feval(b);
}

function _shiftright(a,b){
    return Sapl.feval(a) >>> Sapl.feval(b);
}

// --------- Function overrides -----------------------------

function __sapldebug_sapldebug(a,b){
	console.log("DEBUG: "+Sapl.toString(a));
	return b;
}

function __iTasks_Framework_ClientSupport_ClientOverride_onClient(){
	return true;
}

function __iTasks_Framework_ClientSupport_ClientOverride_cast_to_TaskValue(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __iTasks_Framework_ClientSupport_ClientOverride_cast(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __dynamic_string_copy_to_string(a){
	return Sapl.dynamicToString(Sapl.feval(a));
}

function __dynamic_string_copy_from_string(a){
	eval("var tmp="+Sapl.feval(a)+";");
	return ___predefined__Tuple2(tmp, a); // TODO: second?
}

function __Text_Encodings_Base64_base64Encode(a){
	return window.btoa(Sapl.feval(a));
}

function __Text_Encodings_Base64_base64Decode(a){
	return window.atob(Sapl.feval(a));
}
