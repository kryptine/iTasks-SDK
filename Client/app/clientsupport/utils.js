isArray = function(o){
	return (o instanceof Array);
}

isNumber = function(o){
	return (typeof o == "number");
}

isString = function(o){
	return (typeof o == "string");
}

isBoolean = function(o){
	return (typeof o == "boolean");
}

isFunction = function(o){
	return (typeof o == "function");
}

String.prototype.trim = function () {
    return this.replace(/^\s*/, "").replace(/\s*$/, "");
}

String.prototype.startsWith = function(str){
    return (this.indexOf(str) === 0);
}

String.prototype.endsWith = function(str){
    var lastIndex = this.lastIndexOf(str);
    return (lastIndex != -1) && (lastIndex + str.length == this.length);
}

String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

/* Only this way works for built in javascript objects */
function streval(obj, method){
    var str = "obj."+method+"(";

    // Creating a closure
    var eventHandler = function(expr){
		
		var h = function(param){
			expr[1].push(param);
			Sapl.feval(expr);
		};
		
		return h;
    }
	    
    for(var i=2; i<arguments.length; i++){
	// if an argument is a function, we handle it as an event handler
        if(isArray(arguments[i]) && isFunction(arguments[i][0])){
	      arguments[i] = eventHandler(arguments[i]);
	}
	    
        if(i>2) str += ",";
        str += "arguments["+i+"]";
    }
    
    str = str + ")";
    var value = eval(str);
    
    return value;
}

function isIE(){
    return /msie/i.test(navigator.userAgent) && !/opera/i.test(navigator.userAgent);
}

//This prototype is provided by the Mozilla foundation and
//is distributed under the MIT license.
//http://www.ibiblio.org/pub/Linux/LICENSES/mit.license
if (!Array.prototype.map)
{
  Array.prototype.map = function(fun /*, thisp*/)
  {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var res = new Array(len);
    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this)
        res[i] = fun.call(thisp, this[i], i, this);
    }

    return res;
  };
}

//This prototype is provided by the Mozilla foundation and
//is distributed under the MIT license.
//http://www.ibiblio.org/pub/Linux/LICENSES/mit.license

if (!Array.prototype.some)
{
  Array.prototype.some = function(fun /*, thisp*/)
  {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this &&
          fun.call(thisp, this[i], i, this))
        return true;
    }

    return false;
  };
}

if (!Array.prototype.any)
{
  Array.prototype.any = function(element /*, thisp*/)
  {
    return this.some(function(a){return a==element;})
  };
}

(evalScript = function(e){
	var	h = evalScript.node,
		s = document.createElement("script");
	s.type = "text/javascript";
	/*@cc_on if(!(s.text=e))@*/
	s.appendChild(document.createTextNode(e));
	h.appendChild(s);
	h.removeChild(s);
}).node = document.getElementsByTagName("head")[0] || document.getElementsByTagName("*")[0];
