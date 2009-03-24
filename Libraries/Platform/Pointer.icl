implementation module Pointer

import StdOverloaded, StdArray, StdInt, StdChar, StdString
/*
   0:   8b 04 18                mov    (%eax,%ebx,1),%eax
   3:   0f b7 04 18             movzwl (%eax,%ebx,1),%eax
   7:   0f b6 04 18             movzbl (%eax,%ebx,1),%eax
   b:   0f bf 04 18             movswl (%eax,%ebx,1),%eax
   f:   0f be 04 18             movsbl (%eax,%ebx,1),%eax
  13:   dd 04 18                fldl   (%eax,%ebx,1)
  16:   dd d9                   fstp   %st(1)
  18:   d9 04 18                flds   (%eax,%ebx,1)
  1b:   dd d9                   fstp   %st(1)
  1d:   dd 14 18                fstl   (%eax,%ebx,1)
  20:   d9 14 18                fsts   (%eax,%ebx,1)
  23:   89 d8                   mov    %ebx,%eax
  25:   59                      pop    %ecx
  26:   89 0c 18                mov    %ecx,(%eax,%ebx,1)
  29:   66 89 0c 18             mov    %cx,(%eax,%ebx,1)
  2d:   88 0c 18                mov    %cl,(%eax,%ebx,1)
  30:   8b 0c 24                mov    (%esp),%ecx
*/

readInt :: !Pointer !Offset -> Int
readInt pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readWordZ :: !Pointer !Offset -> Int
readWordZ pointer offset = code {
		pop_b 1
|		movzwl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 183
	 	instruction 4
	 	instruction 24
}

readWordS :: !Pointer !Offset -> Int
readWordS pointer offset = code {
		pop_b 1
|		movswl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 191
	 	instruction 4
	 	instruction 24
}

readByteZ :: !Pointer !Offset -> Int
readByteZ pointer offset = code {
		pop_b 1
|		movzwl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readByteS :: !Pointer !Offset -> Int
readByteS pointer offset = code {
		pop_b 1
|		movswl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 190
	 	instruction 4
	 	instruction 24
}

readChar :: !Pointer !Offset -> Char
readChar pointer offset = code {
		pop_b 1
|		movzbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readReal8 :: !Pointer !Offset -> Real
readReal8 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f8_p_
		:read_f8_p_		
|		fldl   (%eax,%ebx,1)
		instruction 221
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

readReal4 :: !Pointer !Offset -> Real
readReal4 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f4_p_
		:read_f4_p_	
|		flds   (%eax,%ebx,1)
		instruction 217
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

writeInt :: !Pointer !Offset !Int -> Int
writeInt pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%eax,%ebx,1)
		instruction 137
		instruction 12
		instruction 24
}

writeWord :: !Pointer !Offset !Int -> Int
writeWord pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movw	%cx,(%eax,%ebx,1)
		instruction 102
		instruction 137
		instruction 12
		instruction 24
}

writeByte :: !Pointer !Offset !Int -> Int
writeByte pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeChar :: !Pointer !Offset !Char -> Int
writeChar pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeReal8 :: !Pointer !Offset !Real -> Int
writeReal8 pointer offset double = code {
		updatepop_b 0 3
|		fstl  (%eax,%ebx,1)
		instruction	221
		instruction 20
		instruction 24
}

writeReal4 :: !Pointer !Offset !Real -> Int
writeReal4 pointer offset double = code {
		updatepop_b 0 3
|		fsts  (%eax,%ebx,1)
		instruction	217
		instruction 20
		instruction 24
}


derefInt :: !Pointer -> Int
derefInt ptr = readInt ptr 0

derefString :: !Pointer -> String
derefString ptr = copy ptr 0 (createArray len '\0')
where
	len = strlen ptr 0

	strlen ptr off
		| readChar ptr off == '\0'	= off
									= strlen ptr (off + 1)

	copy :: !Pointer !Offset *{#Char} -> *{#Char}
	copy ptr off arr
		# char = readChar ptr off
		| char == '\0'	= arr
						= copy ptr (off + 1) {arr & [off] = char}

packInt :: !Int -> {#Int}
packInt i = {i}

packString :: !String -> {#Char}
packString s = s +++ "\0"
