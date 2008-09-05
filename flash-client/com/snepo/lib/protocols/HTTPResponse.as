/* 
//
//	FILE:				HTTPResponse.as
//
//	DESCRIPTION:		Generic HTTP response class (used for setting/getting HTTP headers)
//
//	REVISION:			2006 09 18	v1.2.1 alpha
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

class com.snepo.lib.protocols.HTTPResponse {
	
	//// VARIABLE DECLARATIONS
	
	private var _header:Object;
	
	//// CONSTRUCTOR
	
	function HTTPResponse() {
		_header = {};
	}
	
	//// GETTERS/SETTERS

	public function setHeader(name:String, value:String) {
		_header[name] = value;
	}
	public function getHeader(name:String) {
		return _header[name];
	}
}
