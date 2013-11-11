_feval = function (expr) { // check on length split for == and <=
		var y, f, xs;
		while (1) {
			if (typeof (expr) === "object") {
				if (expr.length == 1) return expr[0]; // boxed primitive
				else if (typeof (expr[0]) === "function") { // closure
					f = expr[0];
					xs = expr[1];
					if (f.length === xs.length) { // most often occuring case
						y = f.apply(this, xs);
						expr[0] = y;
						expr.length = 1;
						return y;
					} else if (f.length < xs.length) { // less likely case
						y = f.apply(this, xs.splice(0, f.length));
						expr[0] = y;
					} else // not enough args
					return expr;
		 		} else if (typeof (expr[0]) === "object") { // curried application -> uncurry
					y = expr[0];
					expr[0] = y[0];
					expr[1] = y[1].concat(expr[1]);
				} else return expr; // constructor
			} else if (typeof (expr) == "function"){
				if (expr.length == 0) return expr.apply(this, []);
				expr = [expr, []];
			}
			else // simple value
			return expr;
		}
	};

_fapp = function (f,xs){
	//This is increadibnly slow:
	//xs = Array.prototype.slice.call(arguments, 1);
	
	while(1){
		if (typeof f === "function"){
			if (f.length === xs.length) { // most often occuring case
				return f.apply(this, xs);
			} else if (f.length < xs.length) { // less likely case
				f = f.apply(this, xs.splice(0, f.length));
			} else // not enough args: convert it thunk
				return [f,xs];
		// typeof f == "object"
		} else { // curried application -> uncurry
			xs = f[1].concat(xs);
			f = f[0];			
		}
	}
}

