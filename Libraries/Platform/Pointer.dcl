definition module Pointer
/**
	Low level reading from and writing to memory using pointers and offsets.
*/

:: Pointer	:== Int
:: Offset	:== Int

/**
* Read an integer (32 bits)
*/
readInt		:: !Pointer !Offset -> Int
/**
* Read a word (16 bits) zero extended
*/
readWordZ	:: !Pointer !Offset -> Int
/**
* Read a word (16 bits) sign extended
*/
readWordS	:: !Pointer !Offset -> Int
/**
* Read a byte (8 bits) zero extended
*/
readByteZ	:: !Pointer !Offset -> Int
/**
* Read a byte (8 bits) sign extended
*/
readByteS	:: !Pointer !Offset -> Int
/**
* Read a char
*/
readChar	:: !Pointer !Offset -> Char
/**
* Read a real (8 bytes)
*/
readReal8	:: !Pointer !Offset -> Real
/**
* Read a real (4 bytes)
*/
readReal4	:: !Pointer !Offset -> Real

/**
* Write an integer (32 bits)
*/
writeInt	:: !Pointer !Offset !Int -> Int
/**
* Write a word (16 bits)
*/
writeWord	:: !Pointer !Offset !Int -> Int
/**
* Write a word (8 bits)
*/
writeByte	:: !Pointer !Offset !Int -> Int
/**
* Write a char 
*/
writeChar	:: !Pointer !Offset !Char -> Int
/**
* Write a real (8 bytes) 
*/
writeReal8	:: !Pointer !Offset !Real -> Int
/**
* Write a real (4 bytes)
*/
writeReal4	:: !Pointer !Offset !Real -> Int


//Utility functions

/**
* Reads the integer located at the pointer
*/
derefInt :: !Pointer -> Int
/**
* Reads the NUL-terminated C-string indicated by the pointer and
* converts it to a normal (not NUL-terminated) Clean-string
*/
derefString :: !Pointer -> String
/**
* Wraps an integer in an array to enable passing a pointer instead
* of a value to a ccall.
*/
packInt :: !Int -> {#Int}
/**
* Wraps a Clean-string as a NUL-terminated C-string to enable passing
* a pointer to a ccall using the C conventions.
*/
packString :: !String -> {#Char}
