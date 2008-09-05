/* 
//
//	FILE:				HTTPPut.as
//
//	DESCRIPTION:		HTTP PUT Class (handles HTTP PUT requests and extends the generic HTTPRequest class)
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

import com.snepo.lib.protocols.HTTPRequest;
import com.snepo.lib.iso.ISO;

class com.snepo.lib.protocols.HTTPPut extends HTTPRequest{
		
	//// VARIABLE DECLARATIONS
	
	private var _url:String;
	private var _data:String;
	private var _id:String;
	private var _request:String
	private var _requestType:String = "PUT";
		
	//// CONSTRUCTOR
	
	function HTTPPut(url:String, data:String, id:String) {
		_url = url;
		_data = data;
		_id = id;
	}
		
	//// INTERFACE METHOD IMPLIMENTATIONS
	
	function toString(Void):String{
		if(_data != undefined) { // if store (else create)
			this.setHeader("X-Creation-Date", ISO.dateToString(new Date()));
			this.setHeader("X-Owner", getOwner());
			this.setHeader("Content-Length", _data.length.toString());
		}
		setRequest() 
		var sendStr = _requestType+" "+_request;
		sendStr += " HTTP/1.1\r\n"
		sendStr += this.getHeaders();
		if(_data != undefined) sendStr += "\r\n"+_data+"\r\n";
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
		
	//// UTILITY METHODS
	
	private function getOwner(Void):String {
		var u = _url.split("\\").join("/").split("/");
		return unescape(u[u.length-1]);
	}	
}
