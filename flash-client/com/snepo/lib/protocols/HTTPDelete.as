/* 
//
//	FILE:				HTTPDelete.as
//
//	DESCRIPTION:	
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

import com.snepo.lib.protocols.HTTPRequest;

class com.snepo.lib.protocols.HTTPDelete extends HTTPRequest{
	
	//// VARIABLE DECLARATIONS
	
	private var _url:String;
	private var _id:String;
	private var _request:String
	private var _requestType:String = "DELETE";
	
	//// CONSTRUCTOR
	
	function HTTPDelete(url:String, id:String) {
		_url = url;
		_id = id;
	}
	
	//// INTERFACE METHOD IMPLIMENTATIONS
	
	function toString(Void):String{
		setRequest();
		var sendStr = _requestType+" "+_request;
		sendStr += " HTTP/1.1\r\n"
		sendStr += this.getHeaders();
		sendStr += "\r\n";
		return sendStr;
	};
	
	function setRequest() {
		_request = _url;
		if(_id != undefined) _request +="?id="+_id;
	}
	
	function getRequestHeader():String{
		setRequest();
		return _requestType+":"+_request;
	}
	
	function getRequestType():String{
		return _requestType;
	}
}