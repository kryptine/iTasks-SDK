"use strict";

var ABC_DEBUG=false;

function SharedCleanValue(index) {
	return {
		shared_clean_value_index: index
	};
}

const abc_interpreter={
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
		abc_interpreter.log_buffer+=s;
		if (s.indexOf('\n')>=0) {
			var lines=abc_interpreter.log_buffer.split('\n');
			for (var i=0; i<lines.length-1; i++)
				console.log(lines[i]);
			abc_interpreter.log_buffer=lines[lines.length-1];
		}
	},
	empty_log_buffer: function(){
		if (abc_interpreter.log_buffer.length>0)
			console.log(abc_interpreter.log_buffer);
	},

	deserialize: function(string) {
		var array=new Int8Array(string.length);
		for (var i in string)
			array[i]=string.charCodeAt(i);
		var graph=new Uint32Array(array.buffer);
		var unused_semispace=abc_interpreter.util.instance.exports.get_unused_semispace();
		for (var i=0; i<graph.length; i++)
			abc_interpreter.memory_array[unused_semispace/4+i] = graph[i];

		// TODO: maybe garbage collection is needed
		var old_hp=abc_interpreter.interpreter.instance.exports.get_hp();
		var new_hp=abc_interpreter.util.instance.exports.copy_from_string(
			unused_semispace,
			graph.length/2,
			abc_interpreter.interpreter.instance.exports.get_asp()+8,
			abc_interpreter.interpreter.instance.exports.get_bsp()-8,
			old_hp,
			abc_interpreter.code_offset*8);
		abc_interpreter.interpreter.instance.exports.set_hp(new_hp);
		abc_interpreter.interpreter.instance.exports.set_hp_free(
			abc_interpreter.interpreter.instance.exports.get_hp_free()-(new_hp-old_hp)/8);

		var index=abc_interpreter.shared_clean_values.length;
		abc_interpreter.shared_clean_values.push(abc_interpreter.memory_array[unused_semispace/4]);

		return SharedCleanValue(index);
	},

	interpret: null,

	shared_js_values: [], // javascript objects accessible from Clean
	shared_clean_values: [], // pointers to the Clean heap

	apply_to_clean_value: function (index) {
		var f=function () {
			var args=[];
			for (var i=0; i<arguments.length; i++)
				args[i]=arguments[i];
			abc_interpreter.interpret(SharedCleanValue(index), args);
		};
		f.shared_clean_value_index=index;
		return f;
	},

	copy_js_to_clean: function (values, store_ptrs, hp, hp_free) {
		for (var i=0; i<values.length; i++) {
			if (values[i]===null) {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=26*8+2; // INT
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=0;
				abc_interpreter.memory_array[hp/4+3]=1<<30;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='undefined') {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=26*8+2; // INT
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=1;
				abc_interpreter.memory_array[hp/4+3]=1<<30;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='number') {
				// TODO use small integers
				// TODO check garbage collection
				if (Number.isInteger(values[i])) {
					abc_interpreter.memory_array[store_ptrs/4]=hp;
					abc_interpreter.memory_array[hp/4]=26*8+2; // INT
					abc_interpreter.memory_array[hp/4+1]=0;
					abc_interpreter.memory_array[hp/4+2]=values[i]; // TODO also support >32-bit
					abc_interpreter.memory_array[hp/4+3]=0;
					hp+=16;
					hp_free-=2;
				} else {
					abc_interpreter.memory_array[store_ptrs/4]=hp;
					abc_interpreter.memory_array[hp/4]=21*8+2; // REAL
					abc_interpreter.memory_array[hp/4+1]=0;
					const float_array=new Float64Array(abc_interpreter.memory_array.buffer, hp+8);
					float_array[0]=values[i];
					hp+=16;
					hp_free-=2;
				}
			} else if (typeof values[i]=='boolean') {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=11*8+2; // BOOL
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=values[i] ? 1 : 0;
				abc_interpreter.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='string') {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=6*8+2; // _STRING_
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=values[i].length;
				abc_interpreter.memory_array[hp/4+3]=0;
				var array=new Int8Array(((values[i].length+3)>>2)<<2);
				for (var j in values[i])
					array[j]=values[i].charCodeAt(j);
				array=new Uint32Array(array.buffer);
				for (var j=0; j<((values[i].length+3)>>2); j++)
					abc_interpreter.memory_array[hp/4+4+j]=array[j];
				hp+=16+(((values[i].length+7)>>3)<<3);
				hp_free-=2+((values[i].length+7)>>3);
			} else if (Array.isArray(values[i])) {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=2; // fake ARRAY, needed because we use jmp_ap
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=hp+16;
				abc_interpreter.memory_array[hp/4+3]=0;
				abc_interpreter.memory_array[hp/4+4]=1*8+2; // _ARRAY_
				abc_interpreter.memory_array[hp/4+5]=0;
				abc_interpreter.memory_array[hp/4+6]=values[i].length;
				abc_interpreter.memory_array[hp/4+7]=0;
				abc_interpreter.memory_array[hp/4+8]=0;
				abc_interpreter.memory_array[hp/4+9]=0;
				hp+=40;
				hp_free-=5;
				var copied=abc_interpreter.copy_js_to_clean(values[i], hp, hp+8*values[i].length, hp_free);
				hp=copied.hp;
				hp_free=copied.hp_free-values[i].length;
			} else if ('shared_clean_value_index' in values[i]) {
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=661*8+2; // DOMNode type
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=(values[i].shared_clean_value_index<<1)+1;
				abc_interpreter.memory_array[hp/4+3]=0;
			} else if (typeof values[i]=='object') {
				// TODO: check if garbage collection is needed
				abc_interpreter.memory_array[store_ptrs/4]=hp;
				abc_interpreter.memory_array[hp/4]=661*8+2; // DOMNode type
				abc_interpreter.memory_array[hp/4+1]=0;
				abc_interpreter.memory_array[hp/4+2]=abc_interpreter.shared_js_values.length<<1;
				abc_interpreter.memory_array[hp/4+3]=0;
				abc_interpreter.shared_js_values.push(values[i]);
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
		var size=abc_interpreter.memory_array[hp_ptr/4+2];
		var string_buffer=new Uint8Array(abc_interpreter.memory.buffer, hp_ptr+16);
		var string='';
		for (var i=0; i<size; i++)
			string+=String.fromCharCode(string_buffer[i]);
		return string;
	},
};

abc_interpreter.loading_promise=fetch('js/app.pbc').then(function(resp){
	if (!resp.ok)
		throw 'failed to fetch bytecode';
	return resp.arrayBuffer();
}).then(function(bytecode){
	abc_interpreter.prog=new Uint32Array(bytecode);

	const blocks_needed=Math.floor((abc_interpreter.prog.length*4 + abc_interpreter.stack_size + abc_interpreter.hp_size*2 + 65535) / 65536);

	abc_interpreter.memory=new WebAssembly.Memory({initial: blocks_needed});
	abc_interpreter.memory_array=new Uint32Array(abc_interpreter.memory.buffer);

	for (var i in abc_interpreter.prog)
		abc_interpreter.memory_array[i]=abc_interpreter.prog[i];

	(function(prog){
		var i=0;
		while (prog.length > 0) {
			if (prog[0]==1) /* ST_Code section; see ABCInterpreter's bcprelink.c */
				abc_interpreter.code_offset=i+1;
			i+=1+prog[1];
			prog=prog.slice(2+2*prog[1]);
		}
	})(abc_interpreter.prog);

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter-util.wasm'),
		{ clean: {
			memory: abc_interpreter.memory,
			has_host_reference: function (index) {
				if (abc_interpreter.shared_clean_values.length>index && abc_interpreter.shared_clean_values[index]!=null)
					return abc_interpreter.shared_clean_values[index];
				return 0;
			},
			update_host_reference: function (index, new_location) {
				abc_interpreter.shared_clean_values[index]=new_location;
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
	abc_interpreter.util=util;

	const interpreter_imports={
		clean: {
			memory: abc_interpreter.memory,

			debug_instr: function (addr, instr) {
				if (MAX_INSTRUCTIONS-- == 0)
					throw 'MAX_INSTRUCTIONS ran out';
				if (ABC_DEBUG)
					console.log(addr/8-abc_interpreter.code_offset,abc_instructions[instr]);
			},
			handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp, hp_free) {
				if (abc_instructions[instr]=='instruction') {
					const arg=abc_interpreter.memory_array[(pc+8)/4];
					switch (arg) {
						case 0: /* evaluation finished */
							return 0;
						case 1: /* iTasks.UI.JS.Interface: eval_js */
							var string=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							Function(string)();
							break;
						case 2:
							var string=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							var result=eval('('+string+')'); // the parentheses are needed for {}, for instance
							var copied=abc_interpreter.copy_js_to_clean([result], asp, hp, hp_free);
							abc_interpreter.interpreter.instance.exports.set_hp(copied.hp);
							abc_interpreter.interpreter.instance.exports.set_hp_free(copied.hp_free);
							break;
						case 3: /* iTasks.UI.JS.Interface: share */
							abc_interpreter.memory_array[bsp/4]=abc_interpreter.shared_clean_values.length;
							abc_interpreter.shared_clean_values.push(abc_interpreter.memory_array[asp/4]);
							break;
						case 4: /* iTasks.UI.JS.Interface: fetch */
							var index=abc_interpreter.memory_array[bsp/4];
							abc_interpreter.memory_array[asp/4]=abc_interpreter.shared_clean_values[index];
							break;
						case 5: /* iTasks.UI.JS.Interface: deserialize */
							var string=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4]);
							var shared_clean_value=abc_interpreter.deserialize(string);
							abc_interpreter.memory_array[asp/4]=abc_interpreter.shared_clean_values[shared_clean_value.shared_clean_value_index];
							break;
						case 10: /* iTasks.UI.JS.Interface: add CSS */
							var url=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4]);
							var css=document.createElement('link');
							css.rel='stylesheet';
							css.type='text/css';
							css.async=true;
							css.href=url;
							document.head.appendChild(css);
							break;
						case 11: /* iTasks.UI.JS.Interface: add JS */
							var url=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4]);
							var callback=abc_interpreter.get_clean_string(abc_interpreter.memory_array[asp/4-2]);
							var js=document.createElement('script');
							js.type='text/javascript';
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
				abc_interpreter.empty_log_buffer();
				if (abc_instructions[instr]=='instruction')
					/* `instruction 0` ends the interpretation, so this is no error */
					return;
				throw ('illegal instruction '+instr+' ('+abc_instructions[instr]+') at address '+(addr/8-1));
			},
			out_of_memory: function () {
				abc_interpreter.empty_log_buffer();
				throw 'out of memory';
			},
			gc: util.instance.exports.gc,
			halt: function (pc, hp_free, hp_size) {
				abc_interpreter.empty_log_buffer();
				throw ('halt at '+((pc/8)-abc_interpreter.code_offset));
			},

			memcpy: util.instance.exports.memcpy,
			strncmp: util.instance.exports.strncmp,

			putchar: function (v) {
				abc_interpreter.log(String.fromCharCode(v));
			},
			print_int: function (high,low) {
				if (high==0 && low>=0) {
					abc_interpreter.log(low);
				} else {
					var n=BigInt(high)*BigInt(2)**BigInt(32);
					if (low<0) {
						n+=BigInt(2)**BigInt(31);
						low+=2**31;
					}
					n+=BigInt(low);
					abc_interpreter.log(n);
				}
			},
			print_bool: function (v) {
				abc_interpreter.log(v==0 ? 'False' : 'True');
			},
			print_char: function (v) {
				abc_interpreter.log("'"+String.fromCharCode(v)+"'");
			},
			print_real: function (v) {
				abc_interpreter.log(Number(0+v).toLocaleString(
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
				abc_interpreter.memory_array[dest/4]=6*8+2; // __STRING__
				abc_interpreter.memory_array[dest/4+1]=0;
				abc_interpreter.memory_array[dest/4+2]=v.length;
				abc_interpreter.memory_array[dest/4+3]=0;
				var arr=new Uint8Array(abc_interpreter.memory_array.buffer, dest+16);
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
	abc_interpreter.interpreter=intp;

	const asp=4*abc_interpreter.prog.length;
	const bsp=asp+abc_interpreter.stack_size;
	const csp=asp+abc_interpreter.stack_size/2;
	const hp=bsp+8;

	abc_interpreter.util.instance.exports.setup_gc(hp, abc_interpreter.hp_size, asp, 98*8);

	abc_interpreter.interpreter.instance.exports.set_asp(asp);
	abc_interpreter.interpreter.instance.exports.set_bsp(bsp);
	abc_interpreter.interpreter.instance.exports.set_csp(csp);
	abc_interpreter.interpreter.instance.exports.set_hp(hp);
	abc_interpreter.interpreter.instance.exports.set_hp_free(abc_interpreter.hp_size/8);
	abc_interpreter.interpreter.instance.exports.set_hp_size(abc_interpreter.hp_size);

	abc_interpreter.interpret=function (f, args) {
		var asp=abc_interpreter.interpreter.instance.exports.get_asp();
		const old_asp=asp;
		var hp=abc_interpreter.interpreter.instance.exports.get_hp();
		var hp_free=abc_interpreter.interpreter.instance.exports.get_hp_free();

		abc_interpreter.memory_array[asp/4]=(31+17*2)*8; // JSWorld: INT 17
		abc_interpreter.memory_array[asp/4+2]=hp;
		abc_interpreter.memory_array[asp/4+4]=abc_interpreter.shared_clean_values[f.shared_clean_value_index];

		const copied=abc_interpreter.copy_js_to_clean([args], asp+8, hp, hp_free);
		asp+=16;
		hp=copied.hp;
		hp_free=copied.hp_free;

		var csp=abc_interpreter.interpreter.instance.exports.get_csp();
		abc_interpreter.memory_array[csp/4]=659*8; // instruction 0; to return
		csp+=8;

		const old_pc=abc_interpreter.interpreter.instance.exports.get_pc();
		abc_interpreter.interpreter.instance.exports.set_pc(100*8); // jmp_ap2
		abc_interpreter.interpreter.instance.exports.set_asp(asp);
		abc_interpreter.interpreter.instance.exports.set_csp(csp);
		abc_interpreter.interpreter.instance.exports.set_hp(hp);
		abc_interpreter.interpreter.instance.exports.set_hp_free(hp_free);

		abc_interpreter.interpreter.instance.exports.interpret();

		abc_interpreter.interpreter.instance.exports.set_pc(old_pc);
		abc_interpreter.interpreter.instance.exports.set_asp(old_asp);
	};
});
