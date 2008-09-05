/* 
//
//	FILE:				Stack.as
//
//	DESCRIPTION:		Stack manager (currently instantiates an array and uses native array methods - will be extended at a later date)
//
//	REVISION:			2006 09 18	v1.2.1 alpha		
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

class com.snepo.lib.managers.Stack {

	//// VARIABLE DECLARATIONS
	
	private var _stack:Array;
	private var lastRequest:Object;
	
	//// CONSTRUCTOR
	
	function Stack() {
		_stack = [];
	};
	
	//// PUBLIC METHODS
	
	public function push(obj):Number {
		return _stack.push(obj);
	}
	public function pop():Object {
		lastRequest = _stack.shift();
		return lastRequest;
	}
	public function get length():Number {
		return _stack.length;
	}
	public function get _last():Object {
		return lastRequest;
	}
}
