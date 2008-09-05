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
//// IMPORTS

import com.snepo.lib.utils.md5;

class com.snepo.lib.authentication.AuthenticationHeader {

	//// VARIABLE DECLARATIONS
	
	private var _header:Object;
	private static var _md5:md5;
	private var _cache:Object =  {};

	//// CONSTRUCTOR
	
	function AuthenticationHeader(header) {
		_md5 = new md5();
	};
	
	//// PUBLIC METHODS
	
	public function setHeaders(header) {
		_header = {};
		var temp_ar = header.substr(7).split(",");
		for(var i=0;i<temp_ar.length;i++) {
			var tempitem = temp_ar[i].split("=");
			_header[tempitem[0]] = tempitem[1].split("\"").join("");
		}
		_cache[_header["realm"]].cnonce
		_header.cnonce = _cache[_header["realm"]].cnonce = random(100000000000);
		_header.nc = _cache[_header["realm"]].nc =  00000001; 
	}	
	
	public function setHeader(name:String, value:String) {
		_header[name] = value;
	}
	public function getHeader(name:String) {
		return _header[name];
	}
	public function getAuthHeader(request) {
		if(request._url) {
			_header["realm"] = request._url.substr(1);
			if(request._username) {
				_header["uri"] =request._url+"?user-name="+request._username;
			} else if(request._id){
				_header["uri"] =request._url+"?id="+request._id;
			} else {
				_header["uri"] =request._url;
			}
		}
		var HA1 = _md5.hash(_header["username"]+":"+_header["realm"]+":"+_header["password"]);
		_cache[_header["realm"]].HA1 =HA1;
		var HA2 = _md5.hash(request.getRequestHeader());
		_header["response"] = _md5.hash(HA1+":"+_header["nonce"]+":"+_header.nc.toString(16)+":"+_header.cnonce+":"+_header["qop"]+":"+HA2);
		var authHeader = "Digest ";
		for(var me in _header) {
			if(me == "nc")  authHeader += me+"=\""+_header[me].toString(16)+"\",";
			else if(me!="password") authHeader += me+"=\""+_header[me]+"\",";
		}
		_header.nc++;
		return authHeader.substr(0,authHeader.length-1);
	}	
}
