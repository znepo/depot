/* 
//
//	FILE:				HTTPGet.as
//
//	DESCRIPTION:		HTTP GET Class (handles HTTP GET requests and extends the generic HTTPRequest class)
//
//	REVISION:			2006 09 18	v1.2.1 alpha		
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

import com.snepo.lib.protocols.HTTPRequest;

class com.snepo.lib.protocols.HTTPGet extends HTTPRequest{
	
	//// VARIABLE DECLARATIONS
	
	private var _url:String;
	private var _id:String;
	private var _request:String
	private var _requestType:String = "GET";
	private var _username:String;
	
	//// CONSTRUCTOR
	
	function HTTPGet(url:String, id:String, username:String) {
		_url = url;
		_id = id;
		_username = username;
	}
	
	//// INTERFACE METHOD IMPLIMENTATIONS
	
	function toString(Void):String{
		setRequest() 
		var sendStr = _requestType+" "+_request;
		sendStr += " HTTP/1.1\r\n"
		sendStr += this.getHeaders();
		sendStr += "\r\n";
		return sendStr;
	};
	
	function setRequest() {
		_request = _url;
		if(_id != undefined) _request +="?id="+_id;
		else if(_username != undefined) _request +="?user-name="+_username;
	}

	function getRequestHeader():String{
		setRequest();
		return _requestType+":"+_request;
	}
	
	function getRequestType():String{
		return _requestType;
	}
	
}
