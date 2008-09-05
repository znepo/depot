/* 
//
//	FILE:				HTTPRequest.as
//
//	DESCRIPTION:		Generic HTTP request class (used for setting/getting HTTP headers and converting headers to string)
//
//	REVISION:			2006 09 18	v1.2.1 alpha		
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

import com.snepo.lib.protocols.Request;

class com.snepo.lib.protocols.HTTPRequest implements Request{
		
	//// VARIABLE DECLARATIONS
	
	private var _header:Object;
		
	//// CONSTRUCTOR
	
	function HTTPRequest(url:String) {
		_header = {};
	}
		
	//// INTERFACE METHOD IMPLIMENTATIONS
	
	function toString(Void):String{
		return "Error: Invocation of abstract class";
	}
	
	function getRequestHeader(Void):String{
		return "Error: Invocation of abstract class";
	}
	
	function getRequestType():String{
		return "Error: Invocation of abstract class";
	}
	
	//// GETTERS/SETTERS
	
	public function setHeader(name:String, value:String) {
		_header[name] = value;
	}
	public function getHeader(name:String) {
		return _header[name];
	}
	public function getHeaders():String {
		var headerString:String = "";
		for(var item in _header) {
			headerString += item+":"+_header[item]+"\r\n";
		}
		return headerString;
	}	
}
