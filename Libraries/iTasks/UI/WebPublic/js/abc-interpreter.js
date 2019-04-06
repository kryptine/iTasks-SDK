"use strict";

const JSWorld={
	abc_type: 'JSWorld'
};

function SharedCleanValue(index) {
	return {
		abc_type: 'SharedCleanValue',
		index: index
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
			abc_interpreter.interpreter.instance.exports.get_asp(),
			abc_interpreter.interpreter.instance.exports.get_bsp(),
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
		return function () {
			var args=[SharedCleanValue(index)];
			for (var i=0; i<arguments.length; i++)
				args[i+1]=arguments[i];
			args.push(JSWorld);
			return abc_interpreter.interpret.apply(null, args);
		};
	},
};

abc_interpreter.loading_promise=fetch('js/app.ubc').then(function(resp){
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
				switch (what) {
					case 0:
						console.log('loop',a,'/',b,'; hp at',c);
						break;
					case 1:
						console.log('desc',a);
						break;
					case 2:
						console.log('hnf, arity',a);
						break;
					case 3:
						console.log('thunk, arity',a);
						break;
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
				console.log(addr,(addr/8-abc_interpreter.code_offset)+'\t'+abc_instructions[instr]);
			},
			handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp) {
				if (abc_instructions[instr]=='instruction') {
					const arg=abc_interpreter.memory_array[(pc+8)/4];
					switch (arg) {
						case 0: /* evaluation finished */
							return 0;
						case 1: /* iTasks.UI.JS.Interface: eval_js */
							var clean_string=abc_interpreter.memory_array[asp/4];
							var size=abc_interpreter.memory_array[clean_string/4+2];
							var string_buffer=new Uint8Array(abc_interpreter.memory.buffer, clean_string+16);
							var string='';
							for (var i=0; i<size; i++)
								string+=String.fromCharCode(string_buffer[i]);
							console.log('eval',string);
							Function(string)();
							break;
						case 2: /* iTasks.UI.JS.Interface: share */
							abc_interpreter.memory_array[bsp/4]=abc_interpreter.shared_clean_values.length;
							abc_interpreter.shared_clean_values.push(abc_interpreter.memory_array[asp/4]);
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

	abc_interpreter.interpret=function(){
		var asp=abc_interpreter.interpreter.instance.exports.get_asp();
		var hp=abc_interpreter.interpreter.instance.exports.get_hp();
		var hp_free=abc_interpreter.interpreter.instance.exports.get_hp_free();

		for (var i=arguments.length-1; i>=0; i--) {
			asp+=8;
			if ('abc_type' in arguments[i]) {
				switch (arguments[i].abc_type) {
					case 'JSWorld':
						abc_interpreter.memory_array[asp/4]=(31+17*2)*8; // INT 17
						break;
					case 'SharedCleanValue':
						abc_interpreter.memory_array[asp/4]=abc_interpreter.shared_clean_values[arguments[i].index];
						break;
					default:
						throw ('unknown abc_type '+arguments[i].abc_type);
				}
			} else if ('domEl' in arguments[i]) { /* probably an iTasks.Component; TODO: come up with a better check for this */
				// TODO: check if garbage collection is needed
				abc_interpreter.memory_array[asp/4]=hp;
				abc_interpreter.memory_array[hp/4]=661*8+2; // DOMNode type
				abc_interpreter.memory_array[hp/4+2]=abc_interpreter.shared_js_values.length;
				abc_interpreter.shared_js_values.push(arguments[i]);
				hp+=16;
				hp_free-=2;
			} else {
				console.log(arguments[i]);
				throw 'Could not pass the above value to Clean';
			}
		}

		var csp=abc_interpreter.interpreter.instance.exports.get_csp();
		abc_interpreter.memory_array[csp/4]=659*8; // instruction 0; to return
		csp+=8;

		var start=(97+arguments.length)*8; // jmp_apn as appropriate; TODO: fix for case where arguments.length=1

		abc_interpreter.interpreter.instance.exports.set_pc(start);
		abc_interpreter.interpreter.instance.exports.set_asp(asp);
		abc_interpreter.interpreter.instance.exports.set_csp(csp);
		abc_interpreter.interpreter.instance.exports.set_hp(hp);
		abc_interpreter.interpreter.instance.exports.set_hp_free(hp_free);

		abc_interpreter.interpreter.instance.exports.interpret();
	};
});
