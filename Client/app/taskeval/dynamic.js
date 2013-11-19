// TO BE SURE
var __Maybe_Nothing = [0, 'Maybe.Nothing'];

function __Maybe_Just(__a1) {
    return [1, 'Maybe.Just', __a1];
};
// TO BE SURE

var ___SystemDynamic_TC_Char = [0, '_SystemDynamic.TC_Char'];
var ___SystemDynamic_TC_Real = [0, '_SystemDynamic.TC_Real'];
var ___SystemDynamic_TC_Int  = [0, '_SystemDynamic.TC_Real'];
var ___SystemDynamic_TC_Bool = [0, '_SystemDynamic.TC_Bool'];
var ___SystemDynamic_TC__List = [0, '_SystemDynamic.TC__List'];
var ___SystemDynamic_TC__UnboxedArray = [0, '_SystemDynamic.TC__UnboxedArray'];
var ___SystemDynamic_TC__Tuple2 = [0, '_SystemDynamic.TC__Tuple2'];
var ___SystemDynamic_TC__Tuple3 = [0, '_SystemDynamic.TC__Tuple3'];
var ___SystemDynamic_TC__Tuple4 = [0, '_SystemDynamic.TC__Tuple4'];

function ___SystemDynamic_TypeCons(__a1) {
    return [2, '_SystemDynamic.TypeCons', __a1];
};

function ___SystemDynamic_TypeApp(__a1, __a2) {
    return [3, '_SystemDynamic.TypeApp', __a1, __a2];
};

function ___SystemDynamic__TypeCodeConstructor(__a1) {
    return [0, '_SystemDynamic._TypeCodeConstructor', __a1];
};

function ___SystemDynamic__DynamicTemp(__a1, __a2) {
    return [0, '_SystemDynamic._DynamicTemp', __a1, __a2];
};

function ___SystemDynamic__initial_unification_environment(n_type_pattern_vars, n_type_vars){
	return undefined; // undefined will do it
};

// Don't do anything
function ___SystemDynamic__normalise(subst, t) { return t; };
function ___SystemDynamic__bind_global_type_pattern_var(t1, t2, subst) { return subst; };

function ___SystemDynamic__unify(subst, t1, t2){
	return ___Tuple2(unify(t1, Sapl.heval(t2), true), subst);
};

// Very simple unification algorithm
function unify(t1, t2, enable_cast){
	// Check constructor name
	if(t1[1] != t2[1]){
	
		// Int can be unified with Real
		if(t1[1] == '_SystemDynamic.TC_Int' && t1[1] == '_SystemDynamic.TC_Real'){
			return true;
		// A Char is also a String in JS
		}else if(enable_cast && unify(t1, charType(), false) && unify(t2, stringType(), false)){
			return true;
		}else{
			return false;
		}
		
	}else{
	
		// Should it be checked?
		if(t1.length != t2.length) return false;
	
		for(var i=2; i<t1.length; i++){
			var r = unify(t1[i], t2[i], enable_cast);
			if(!r) return false;
		}
		
		return true;
	}
}

function applyTypes(arr, containerType){
	var element = arr.pop();
	if(arr.length==0){
		return ___SystemDynamic_TypeApp(containerType, getType(element));
	}else{
		return ___SystemDynamic_TypeApp(applyTypes(arr, containerType), getType(element));
	}
}

function toDynamic(val){
	return ___SystemDynamic__DynamicTemp(val, getType(val));
}

function getType(val){

	if(isNumber(val)){
	
		if(isFloat(val)){
			return floatType();
		}else{
			return intType();
		}
		
	}else if(isString(val)){
	
		return(val.length == 1?charType():stringType());
		
	}else if(isBoolean(val)){
	
		return boolType();
	
	}else if(isArray(val)){
	
		if(val[0] == 0 && val[1].startsWith("_Tuple")){
			return applyTypes(val.slice(2,val.length), tupleType(val.length-2));
		}
	
		return unknownType();
		
	}else{
	
		return unknownType();
	}
}

function floatType(){
	return singleType(___SystemDynamic_TC_Real);
}

function intType(){
	return singleType(___SystemDynamic_TC_Int);
}

function boolType(){
	return singleType(___SystemDynamic_TC_Bool);
}

function charType(){
	return singleType(___SystemDynamic_TC_Char);
}

function tupleType(nr){
	return singleType([0, "_SystemDynamic.TC__Tuple"+nr]);
}

// To avoid unification
function unknownType(){
	return singleType([0, 'unknown_type']);
}	

function singleType(type){
	return ___SystemDynamic_TypeCons(___SystemDynamic__TypeCodeConstructor(type));
}

function stringType(){
	return ___SystemDynamic_TypeApp(
					singleType(___SystemDynamic_TC__UnboxedArray), // Array of
					charType());         							 // characters
}

