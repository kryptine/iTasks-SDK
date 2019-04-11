"use strict";

var ABC_DEBUG=false;

function SharedCleanValue(index) {
	return {
		shared_clean_value_index: index
	};
}

const ABC={
	prog: undefined,
	memory: undefined,
	memory_array: undefined,

	start: undefined,
	code_offset: undefined,

	stack_size: (512<<10)*2,
	hp_size: 2<<20,

	util: undefined,
	interpreter: undefined,

	log_buffer: '',
	log: function (s) {
		s=String(s);
		ABC.log_buffer+=s;
		if (s.indexOf('\n')>=0) {
			var lines=ABC.log_buffer.split('\n');
			for (var i=0; i<lines.length-1; i++)
				console.log(lines[i]);
			ABC.log_buffer=lines[lines.length-1];
		}
	},
	empty_log_buffer: function(){
		if (ABC.log_buffer.length>0)
			console.log(ABC.log_buffer);
	},

	deserialize: function(string) {
		var array=new Int8Array(string.length);
		for (var i in string)
			array[i]=string.charCodeAt(i);
		var graph=new Uint32Array(array.buffer);
		var unused_semispace=ABC.util.instance.exports.get_unused_semispace();
		for (var i=0; i<graph.length; i++)
			ABC.memory_array[unused_semispace/4+i]=graph[i];

		// TODO: maybe garbage collection is needed
		var old_hp=ABC.interpreter.instance.exports.get_hp();
		var new_hp=ABC.util.instance.exports.copy_from_string(
			unused_semispace,
			graph.length/2,
			ABC.interpreter.instance.exports.get_asp()+8,
			ABC.interpreter.instance.exports.get_bsp()-8,
			old_hp,
			ABC.code_offset*8);
		ABC.interpreter.instance.exports.set_hp(new_hp);
		ABC.interpreter.instance.exports.set_hp_free(
			ABC.interpreter.instance.exports.get_hp_free()-(new_hp-old_hp)/8);

		var index=ABC.shared_clean_values.length;
		ABC.shared_clean_values.push(ABC.memory_array[unused_semispace/4]);

		return SharedCleanValue(index);
	},

	interpret: null,

	js: [], // javascript objects accessible from Clean
	shared_clean_values: [], // pointers to the Clean heap

	ap: function (index) { // create a JavaScript closure to call the interpreter
		var f=function () {
			var args=[];
			for (var i=0; i<arguments.length; i++)
				args[i]=arguments[i];
			ABC.interpret(SharedCleanValue(index), args);
		};
		f.shared_clean_value_index=index;
		return f;
	},

	copy_js_to_clean: function (values, store_ptrs, hp, hp_free, wrap_array) {
		for (var i=0; i<values.length; i++) {
			if (values[i]===null) {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=26*8+2; // INT
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=0;
				ABC.memory_array[hp/4+3]=1<<30;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='undefined') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=26*8+2; // INT
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=1;
				ABC.memory_array[hp/4+3]=1<<30;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='number') {
				// TODO use small integers
				// TODO check garbage collection
				if (Number.isInteger(values[i])) {
					ABC.memory_array[store_ptrs/4]=hp;
					ABC.memory_array[hp/4]=26*8+2; // INT
					ABC.memory_array[hp/4+1]=0;
					ABC.memory_array[hp/4+2]=values[i]; // TODO also support >32-bit
					ABC.memory_array[hp/4+3]=0;
					hp+=16;
					hp_free-=2;
				} else {
					ABC.memory_array[store_ptrs/4]=hp;
					ABC.memory_array[hp/4]=21*8+2; // REAL
					ABC.memory_array[hp/4+1]=0;
					const float_array=new Float64Array(ABC.memory_array.buffer, hp+8);
					float_array[0]=values[i];
					hp+=16;
					hp_free-=2;
				}
			} else if (typeof values[i]=='boolean') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=11*8+2; // BOOL
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i] ? 1 : 0;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='string') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=6*8+2; // _STRING_
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i].length;
				ABC.memory_array[hp/4+3]=0;
				var array=new Int8Array(((values[i].length+3)>>2)<<2);
				for (var j in values[i])
					array[j]=values[i].charCodeAt(j);
				array=new Uint32Array(array.buffer);
				for (var j=0; j<((values[i].length+3)>>2); j++)
					ABC.memory_array[hp/4+4+j]=array[j];
				hp+=16+(((values[i].length+7)>>3)<<3);
				hp_free-=2+((values[i].length+7)>>3);
			} else if (Array.isArray(values[i])) {
				ABC.memory_array[store_ptrs/4]=hp;
				if (wrap_array) {
					ABC.memory_array[hp/4]=2;
					ABC.memory_array[hp/4+1]=0;
					ABC.memory_array[hp/4+2]=hp+16;
					ABC.memory_array[hp/4+3]=0;
					hp+=16;
					hp_free-=2;
				}
				ABC.memory_array[hp/4]=1*8+2; // _ARRAY_
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i].length;
				ABC.memory_array[hp/4+3]=0;
				ABC.memory_array[hp/4+4]=0;
				ABC.memory_array[hp/4+5]=0;
				hp+=24;
				hp_free-=3;
				var copied=ABC.copy_js_to_clean(values[i], hp, hp+8*values[i].length, hp_free, false);
				hp=copied.hp;
				hp_free=copied.hp_free-values[i].length;
			} else if ('shared_clean_value_index' in values[i]) {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=661*8+2; // DOMNode type
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=(values[i].shared_clean_value_index<<1)+1;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='object') {
				// TODO: check if garbage collection is needed
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=661*8+2; // DOMNode type
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=ABC.js.length<<1;
				ABC.memory_array[hp/4+3]=0;
				ABC.js.push(values[i]);
				hp+=16;
				hp_free-=2;
			} else {
				console.log(values[i]);
				throw 'Could not pass the above value to Clean';
			}

			store_ptrs+=8;
		}

		return {
			store_ptrs: store_ptrs,
			hp: hp,
			hp_free: hp_free,
		};
	},

	get_clean_string: function (hp_ptr) {
		var size=ABC.memory_array[hp_ptr/4+2];
		var string_buffer=new Uint8Array(ABC.memory.buffer, hp_ptr+16);
		var string='';
		for (var i=0; i<size; i++)
			string+=String.fromCharCode(string_buffer[i]);
		return string;
	},
};

ABC.loading_promise=fetch('js/app.pbc').then(function(resp){
	if (!resp.ok)
		throw 'failed to fetch bytecode';
	return resp.arrayBuffer();
}).then(function(bytecode){
	ABC.prog=new Uint32Array(bytecode);

	const blocks_needed=Math.floor((ABC.prog.length*4 + ABC.stack_size + ABC.hp_size*2 + 65535) / 65536);

	ABC.memory=new WebAssembly.Memory({initial: blocks_needed});
	ABC.memory_array=new Uint32Array(ABC.memory.buffer);

	for (var i in ABC.prog)
		ABC.memory_array[i]=ABC.prog[i];

	(function(prog){
		var i=0;
		while (prog.length > 0) {
			if (prog[0]==1) /* ST_Code section; see ABCInterpreter's bcprelink.c */
				ABC.code_offset=i+1;
			i+=1+prog[1];
			prog=prog.slice(2+2*prog[1]);
		}
	})(ABC.prog);

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter-util.wasm'),
		{ clean: {
			memory: ABC.memory,
			has_host_reference: function (index) {
				if (ABC.shared_clean_values.length>index && ABC.shared_clean_values[index]!=null)
					return ABC.shared_clean_values[index];
				return 0;
			},
			update_host_reference: function (index, new_location) {
				ABC.shared_clean_values[index]=new_location;
			},
			debug: function(what,a,b,c) {
				if (!ABC_DEBUG)
					return;
				switch (what) {
					case 0: console.log('loop',a,'/',b,'; hp at',c); break;
					case 1: console.log('desc',a); break;
					case 2: console.log('hnf, arity',a); break;
					case 3: console.log('thunk, arity',a); break;
				}
			}
		}}
	);
}).then(function(util){
	ABC.util=util;

	const interpreter_imports={
		clean: {
			memory: ABC.memory,

			debug_instr: function (addr, instr) {
				if (ABC_DEBUG)
					console.log(addr/8-ABC.code_offset,abc_instructions[instr]);
			},
			handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp, hp_free) {
				if (abc_instructions[instr]=='instruction') {
					const arg=ABC.memory_array[(pc+8)/4];
					switch (arg) {
						case 0: /* evaluation finished */
							return 0;
						case 1: /* iTasks.UI.JS.Interface: eval_js */
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							Function(string)();
							break;
						case 2:
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							var result=eval('('+string+')'); // the parentheses are needed for {}, for instance
							var copied=ABC.copy_js_to_clean([result], asp, hp, hp_free, false);
							ABC.interpreter.instance.exports.set_hp(copied.hp);
							ABC.interpreter.instance.exports.set_hp_free(copied.hp_free);
							break;
						case 3: /* iTasks.UI.JS.Interface: share */
							ABC.memory_array[bsp/4]=ABC.shared_clean_values.length;
							ABC.shared_clean_values.push(ABC.memory_array[asp/4]);
							break;
						case 4: /* iTasks.UI.JS.Interface: fetch */
							var index=ABC.memory_array[bsp/4];
							ABC.memory_array[asp/4]=ABC.shared_clean_values[index];
							break;
						case 5: /* iTasks.UI.JS.Interface: deserialize */
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var shared_clean_value=ABC.deserialize(string);
							ABC.memory_array[asp/4]=ABC.shared_clean_values[shared_clean_value.shared_clean_value_index];
							break;
						case 10: /* iTasks.UI.JS.Interface: add CSS */
							var url=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var css=document.createElement('link');
							css.rel='stylesheet';
							css.type='text/css';
							css.async=true;
							css.href=url;
							document.head.appendChild(css);
							break;
						case 11: /* iTasks.UI.JS.Interface: add JS */
							var url=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var callback=ABC.get_clean_string(ABC.memory_array[asp/4-2]);
							var js=document.createElement('script');
							js.type='text/javascript';
							js.async=false;
							if (callback.length>0)
								js.onload=Function(callback+'();');
							document.head.appendChild(js);
							js.src=url;
							break;
						default:
							throw ('unknown instruction '+arg);
					}
					return pc+16;
				}
				return 0;
			},
			illegal_instr: function (addr, instr) {
				ABC.empty_log_buffer();
				if (abc_instructions[instr]=='instruction')
					/* `instruction 0` ends the interpretation, so this is no error */
					return;
				throw ('illegal instruction '+instr+' ('+abc_instructions[instr]+') at address '+(addr/8-1));
			},
			out_of_memory: function () {
				ABC.empty_log_buffer();
				throw 'out of memory';
			},
			gc: util.instance.exports.gc,
			halt: function (pc, hp_free, hp_size) {
				ABC.empty_log_buffer();
				throw ('halt at '+((pc/8)-ABC.code_offset));
			},

			memcpy: util.instance.exports.memcpy,
			strncmp: util.instance.exports.strncmp,

			putchar: function (v) {
				ABC.log(String.fromCharCode(v));
			},
			print_int: function (high,low) {
				if (high==0 && low>=0) {
					ABC.log(low);
				} else {
					var n=BigInt(high)*BigInt(2)**BigInt(32);
					if (low<0) {
						n+=BigInt(2)**BigInt(31);
						low+=2**31;
					}
					n+=BigInt(low);
					ABC.log(n);
				}
			},
			print_bool: function (v) {
				ABC.log(v==0 ? 'False' : 'True');
			},
			print_char: function (v) {
				ABC.log("'"+String.fromCharCode(v)+"'");
			},
			print_real: function (v) {
				ABC.log(Number(0+v).toLocaleString(
					['en-US'],
					{
						useGrouping: false,
						maximumSignificantDigits: 15,
					}
				));
			},

			powR: Math.pow,
			acosR: Math.acos,
			asinR: Math.asin,
			atanR: Math.atan,
			cosR: Math.cos,
			sinR: Math.sin,
			tanR: Math.tan,
			expR: Math.exp,
			lnR: Math.log,
			log10R: Math.log10,
			RtoAC: function (dest, v) {
				v=Number(0+v).toLocaleString(
					['en-US'],
					{
						useGrouping: false,
						maximumSignificantDigits: 15,
					}
				);
				ABC.memory_array[dest/4]=6*8+2; // __STRING__
				ABC.memory_array[dest/4+1]=0;
				ABC.memory_array[dest/4+2]=v.length;
				ABC.memory_array[dest/4+3]=0;
				var arr=new Uint8Array(ABC.memory_array.buffer, dest+16);
				for (var i=0; i<v.length; i++)
					arr[i]=v.charCodeAt(i);
				return dest+16+(((v.length+7)>>3)<<3);
			},
		}
	};

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter.wasm'),
		interpreter_imports);
}).then(function(intp){
	ABC.interpreter=intp;

	const asp=4*ABC.prog.length;
	const bsp=asp+ABC.stack_size;
	const csp=asp+ABC.stack_size/2;
	const hp=bsp+8;

	ABC.util.instance.exports.setup_gc(hp, ABC.hp_size, asp, 98*8);

	ABC.interpreter.instance.exports.set_asp(asp);
	ABC.interpreter.instance.exports.set_bsp(bsp);
	ABC.interpreter.instance.exports.set_csp(csp);
	ABC.interpreter.instance.exports.set_hp(hp);
	ABC.interpreter.instance.exports.set_hp_free(ABC.hp_size/8);
	ABC.interpreter.instance.exports.set_hp_size(ABC.hp_size);

	ABC.interpret=function (f, args) {
		var asp=ABC.interpreter.instance.exports.get_asp();
		const old_asp=asp;
		var hp=ABC.interpreter.instance.exports.get_hp();
		var hp_free=ABC.interpreter.instance.exports.get_hp_free();

		ABC.memory_array[asp/4]=(31+17*2)*8; // JSWorld: INT 17
		ABC.memory_array[asp/4+2]=hp;
		ABC.memory_array[asp/4+4]=ABC.shared_clean_values[f.shared_clean_value_index];

		const copied=ABC.copy_js_to_clean([args], asp+8, hp, hp_free, true);
		asp+=16;
		hp=copied.hp;
		hp_free=copied.hp_free;

		var csp=ABC.interpreter.instance.exports.get_csp();
		ABC.memory_array[csp/4]=659*8; // instruction 0; to return
		csp+=8;

		const old_pc=ABC.interpreter.instance.exports.get_pc();
		ABC.interpreter.instance.exports.set_pc(100*8); // jmp_ap2
		ABC.interpreter.instance.exports.set_asp(asp);
		ABC.interpreter.instance.exports.set_csp(csp);
		ABC.interpreter.instance.exports.set_hp(hp);
		ABC.interpreter.instance.exports.set_hp_free(hp_free);

		ABC.interpreter.instance.exports.interpret();

		ABC.interpreter.instance.exports.set_pc(old_pc);
		ABC.interpreter.instance.exports.set_asp(old_asp);
	};
});
