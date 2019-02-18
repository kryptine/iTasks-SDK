const abc_interpreter={
	prog: undefined,
	memory: undefined,
	memory_buffer: undefined,

	start: undefined,
	code_offset: undefined,

	stack_size: (512<<10)*2,
	heap_size:  2<<20,

	asp: undefined,
	bsp: undefined,
	csp: undefined,
	hp:  undefined,

	util: undefined,
	interpreter: undefined,

	apply_to_elem_and_JSWorld: function (f, elem) {
		this.queue.push([f,elem]);
	},
	queue: [],

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
};

fetch('js/app.ubc').then(function(resp){
	if (!resp.ok)
		throw 'failed to fetch bytecode';
	return resp.arrayBuffer();
}).then(function(bytecode){
	abc_interpreter.prog=new Uint32Array(bytecode);

	abc_interpreter.asp=4*abc_interpreter.prog.length;
	abc_interpreter.bsp=abc_interpreter.asp+abc_interpreter.stack_size;
	abc_interpreter.csp=abc_interpreter.asp+abc_interpreter.stack_size/2;
	abc_interpreter.hp=abc_interpreter.bsp+8;

	const blocks_needed=Math.floor((abc_interpreter.prog.length*4 + abc_interpreter.stack_size + abc_interpreter.heap_size*2 + 65535) / 65536);

	abc_interpreter.memory=new WebAssembly.Memory({initial: blocks_needed});
	abc_interpreter.memory_buffer=new Uint32Array(abc_interpreter.memory.buffer);

	for (var i in abc_interpreter.prog)
		abc_interpreter.memory_buffer[i]=abc_interpreter.prog[i];

	(function(prog){
		var i=0;
		while (prog.length > 0) {
			if (prog[0]==1) /* ST_Code section; see ABCInterpreter's bcprelink.c */
				abc_interpreter.code_offset=i+1;
			if (prog[0]==3) /* ST_Start */
				abc_interpreter.start=prog[2];
			i+=1+prog[1];
			prog=prog.slice(2+2*prog[1]);
		}
	})(abc_interpreter.prog);
	if (abc_interpreter.start==undefined)
		throw 'program has no start address'; // TODO start address actually not required here

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter-util.wasm'),
		{ clean: {
			memory: abc_interpreter.memory,
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
	abc_interpreter.util.instance.exports.setup_gc(
		abc_interpreter.hp,
		abc_interpreter.heap_size,
		abc_interpreter.asp,
		98*8);

	const interpreter_imports={
		clean: {
			memory: abc_interpreter.memory,

			debug_instr: function (addr, instr) {
				console.log(addr,(addr/8-abc_interpreter.code_offset)+'\t'+abc_instructions[instr]);
			},
			handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp) {
				if (abc_instructions[instr]=='instruction') {
					switch (abc_interpreter.memory_buffer[(pc+8)/4]) {
						case 0: /* evaluation finished */
							return 0;
						case 1: /* jsEval */
							break;
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

			memcpy: function (dest,src,n) {
				// TODO: optimise; move to wasm
				var mem=new Uint8Array(abc_interpreter.memory_buffer.buffer, abc_interpreter.memory_buffer.byteOffset);
				for (var i=0; i<n; i++)
					mem[dest+i]=mem[src+i];
			},
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

	abc_interpreter.apply_to_elem_and_JSWorld=function(f,elem){
		var f_array=new Int8Array(f.length);
		for (var i in f)
			f_array[i]=f.charCodeAt(i);
		var graph=new Uint32Array(f_array.buffer);
		var unused_semispace=abc_interpreter.util.instance.exports.get_unused_semispace();
		for (var i=0; i<graph.length; i++)
			abc_interpreter.memory_buffer[unused_semispace/4+i] = graph[i];
		var hp=abc_interpreter.util.instance.exports.copy_from_string(
			unused_semispace,
			graph.length/2,
			abc_interpreter.asp,
			abc_interpreter.bsp,
			abc_interpreter.hp, // TODO: use latest hp pointers etc. here
			abc_interpreter.code_offset*8);

		var asp=abc_interpreter.asp+24;
		abc_interpreter.memory_buffer[asp/4-4]=(31+37*2)*8; // INT 37
		abc_interpreter.memory_buffer[asp/4-2]=(31+37*2)*8; // INT 37 TODO: one of these should be a reference to `elem`
		abc_interpreter.memory_buffer[asp/4]=abc_interpreter.memory_buffer[unused_semispace/4];

		var csp=abc_interpreter.csp+8;
		abc_interpreter.memory_buffer[csp/4-2]=659*8; // instruction 0

		var start=100*8; // jmp_ap2

		var r=abc_interpreter.interpreter.instance.exports.interpret(
			start,
			asp,
			abc_interpreter.bsp,
			csp,
			hp,
			abc_interpreter.heap_size/8);
		console.log(r);
	};

	while (abc_interpreter.queue.length) {
		var task=abc_interpreter.queue.shift();
		abc_interpreter.apply_to_elem_and_JSWorld(task[0], task[1]);
	}
});
