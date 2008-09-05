/* 
//
//	FILE:				Depot.as
//
//	DESCRIPTION:		Depot Flash client class
//
//	REVISION:			2006 09 18	v1.2.1 alpha		
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS
	
import mx.utils.Delegate;
import com.snepo.lib.serialization.*;
import com.snepo.lib.managers.Stack;
import com.snepo.lib.utils.Utils;
import com.snepo.lib.utils.ChunkDecoder;
import com.snepo.lib.protocols.*;
import com.snepo.lib.authentication.*;


class com.snepo.depot.Depot {
	
	//// VARIABLE DECLARATIONS
	
	private var _instance:Depot;
	private var _sock:XMLSocket;
	private var _stack:Stack;
	private var _authenticationHeader:AuthenticationHeader;
	private var _chunkDecoder:ChunkDecoder;
	
	private var clientVersion:String = "1.2.1"
	private var _ip:String;
	private var _port:Number;
	private var _auth:Boolean = false;
	private var _debug:Boolean = false;
	private var _tracepath:MovieClip;
	private var _int:Number;
	private var timeout:Number = 4000;
	
	private var _connectionState:Boolean = false;
	private var _onConnect:Function;
	private var _onError:Function;
	private var _onCreateDirectory:Function;
	private var _onGet:Function;
	private var _onCreate:Function;
	private var _onDelete:Function;
	
	//// CONSTRUCTOR
	
	function Depot(i:String, p:Number){
		_authenticationHeader = new AuthenticationHeader();
		_instance = this;
		_stack     = new Stack();
		_ip 	 	= i;
		_port	= p;
		_chunkDecoder = new ChunkDecoder();
		_sock 	= new XMLSocket();
		_sock.onConnect    = Delegate.create(_instance, sockOnConnect);
		_sock.onClose 	= Delegate.create(_instance, sockOnClose);
		_sock.onData   	= Delegate.create(_instance, sockOnData);
	}
	
	//// PUBLIC METHODS
	
	// entry public methods
	public function create(url, pobj, name, ctype, id) {
		var request = new HTTPPut(url, getDepotXML(pobj), id);
		setGenericHeader(request);
		request.setHeader("X-Object-Name", name);
		request.setHeader("Content-Type", "text/snepo.depot."+Utils.getType(pobj)+((ctype)?"."+ctype:""));
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function get(url, id, meta) {
		var request = new HTTPGet(url, id);
		setGenericHeader(request);
		if(meta != undefined) request.setHeader("X-Metadata-Only", true);
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function getFirst(url, meta) {
		var request = new HTTPGet(url, "(min)");
		setGenericHeader(request);
		if(meta != undefined) request.setHeader("X-Metadata-Only", true);
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function getLast(url, meta) {
		var request = new HTTPGet(url, "(max)");
		setGenericHeader(request);
		if(meta != undefined) request.setHeader("X-Metadata-Only", true);
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function update(url, pobj) {
		// TBA
	}
	public function remove(url, id) {
		var request = new HTTPDelete(url, id);
		setGenericHeader(request);
		if(id == undefined) request.setHeader("X-Snepo-Directive", "DELETE_DIR");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	
	// directory public methods
	public function newDirectory(url) {
		var request = new HTTPPut(url);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "CREATE");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function destroy(url) {
		var request = new HTTPDelete(url);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "DESTROY_DIR");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function restore(url) {
		var request = new HTTPGet(url);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "RESTORE_DIR");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
		
	// authentication and permissions public methods
	public function destroyAuthentication() {
		//delete _authenticationHeader;
		_auth = false;
	}
	public function authorize(username, password) {
		_authenticationHeader.setHeader("username",username);
		_authenticationHeader.setHeader("password",password);
		_auth = true;
		
		// HACK
		_authenticationHeader.setHeader("uri", "/"+_authenticationHeader.getHeader("uri"));
		var request = _stack._last;
		if(request) {
			request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
			send(request);
		}
	}
	public function getPermissions(url, id, username) {
		// user is not ocmpulsory and returns list of all perms set on url
		var request = new HTTPGet(url, id, username);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "GET_PERM");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function putPermissions(url, id, username, permissions) {
		var str = "user-name="+username+"&dirs="+permissions+"&entries="+permissions;
		var request = new HTTPPut(url, str,null);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "PUT_PERM");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	public function putUser(username, password) {
		var str = "user-name="+username+"&password="+password;
		var request = new HTTPPut("/com/snepo/auth/users", str,null);
		setGenericHeader(request);
		request.setHeader("X-Snepo-Directive", "PUT_USER");
		if(_auth) 	request.setHeader("Authorization", _authenticationHeader.getAuthHeader(request));
		send(request);
	}
	
	private function setGenericHeader(request) {
		request.setHeader("Host", _ip);
		request.setHeader("X-Snepo-Client", "Snepo Flash Client/"+clientVersion);
		request.setHeader("X-Null-Terminated", "true");
		request.setHeader("Connection", "close");
	}
	
	//// UTILITY METHODS
	
	private function getDepotXML(obj) {
		var str = "";
		var objType = Utils.getType(obj); 
		switch(objType) {
			case 'string':
			case 'number':
			case 'date':
			case 'boolean':
			case 'xml':
				str += "<"+objType+">"+Serializer.serialise(obj)+"</"+objType+">";
				break;
			case 'array':
				str += "<"+objType+" scope='"+_level0+"'>";
				for(var i=0;i<obj.length;i++) {
					str += "<n>";
					str += getDepotXML(obj[i])
					str += "</n>";
				}
				str +="</"+objType+">";
				break;
			case 'object':
				str += "<"+objType+">";
				for(var me in obj) {
					str += "<"+me+">";
					str += getDepotXML(obj[me])
					str += "</"+me+">";
				}
				str +="</"+objType+">";
				break;
			default:
				pError("Depot Error: unsupported object type (getDepotXML). ", 415);
				str += "<unknown>"+obj+"</unknown>";
				break;
		}
		return str;
	}
	private function getDepotObject(xml, raw) {
		var objType = (raw)? "raw": xml.nodeName;
		var data = xml.childNodes;
		var obj;
		switch(objType) {
			case 'string':
			case 'number':
			case 'date':
			case 'boolean':
			case 'xml':
				obj = Marshaler.marshal(data[0], objType)
				break;
			case 'array':
				obj = [];
				for(var i=0;i<data.length;i++) {
					obj[i] = getDepotObject(data[i].firstChild);
				}
				break;
			case 'object':
				obj = {};
				for(var i=0;i<data.length;i++) {
					obj[data[i].nodeName] = getDepotObject(data[i].firstChild);
				}
				break;
			case 'directory-auth':
				obj = {};
				obj["directory-perm"] = [];
				obj["entry-perm"] = [];
				for(var i=0;i<data[0].childNodes.length;i++) {
					obj["directory-perm"].push(data[0].childNodes[i].firstChild)
				}
				for(var i=0;i<data[1].childNodes.length;i++) {
					obj["entry-perm"].push(data[1].childNodes[i].firstChild)
				}
				break;
			case 'raw':
				obj = xml.toString();
				break
			default:
				pError("Depot Error: unsupported object type (getDepotObject). ", 415);
				return xml;
				break;
		}
		
		return obj;
		
	}
	
	//// ERROR MANAGMENT

	private function pError(str:String, code:Number, header:Object, instance:Depot):Void {
		clearInterval(_int)
		if(_onError) _onError(str, code, header, instance);
	}
	
	//// SOCKET MANAGEMENT

	private function sockOnConnect(success:Boolean):Void {
		clearInterval(_int)
		if(success) {
			if(_onConnect) _onConnect(success);
			var sndStr = _stack.pop().toString();
			if(_debug) {
				if(_tracepath) {
					_tracepath.text += "SEND ###################################\n";
					_tracepath.text += sndStr+"\n";
				} else {
					trace("SEND ###################################");
					trace(sndStr);
				}
			}
			_sock.send(sndStr);
		} else {
			_connectionState = false;
			pError("Connection Failure.", 408);
		}
	};
	private function sockOnClose(Void):Void {
		_connectionState = false;
		if(_stack.length>0) {
			// An interval is required here to bypass a Flash Player 9.0.16 bug that causes the
			// player to crash when connecting to a socket immediately after it is closed.
			_int = setInterval(this,"connect", 10);
		};
	};
	private function sockOnData(src:String):Void {
		if(_debug) {
			if(_tracepath) {
				_tracepath.text += "RECIEVED ################################\n";
				_tracepath.text += src+"\n";
			} else {
				trace("RECIEVED ################################");
				trace(src);
			}
		}
		var response = new HTTPResponse();
		
		// get response data
		var tempAr = src.split("\r\n\r\n");
		var data = "";
		for(var i=1;i<tempAr.length;i++) {
			data += tempAr[i];
		}
		
		// set known response headers
		response.setHeader("Data", data);
		tempAr = tempAr[0].split("\r\n");
		response.setHeader("Header", tempAr[0]);
		var Code = parseInt(tempAr[0].substr(9,3));
		response.setHeader("Code", Code);
		var Message = tempAr[0].substr(13,tempAr[0].length-13);
		response.setHeader("Message", Message);
		
		// set unknown response headers
		for(var i=1;i<tempAr.length;i++){
			var temp = tempAr[i].split(":");
			if(temp[1].charAt(0) == " ") temp[1] = temp[1].substr(1);
			response.setHeader(temp[0], temp[1]);
		}
		
		// decode response data if chunked (apache proxy)
		if(response.getHeader("Transfer-Encoding") == "chunked") {
			response.setHeader("Data", _chunkDecoder.decode(data));
		}
		
		var genericMessage = "Unsupported response code";
		switch(Code) { 
			case (100): // Continue 
			case (101): // Switching Protocols 
			case (Code<200): // Informational
				pError(genericMessage, Code);
				break;
			case (200): // Successful retrieve or delete 
				var responseXML = new XML(response.getHeader("Data"));
				var nodes = responseXML.firstChild.childNodes;
				var responseAr = [];
				var metadataAr = [];
				var directoryAr = [];
				for(var i=0;i<nodes.length;i++) {
					var metadata = {};
					for(var j=0;j<nodes[i].firstChild.childNodes.length;j++){
						metadata[nodes[i].firstChild.childNodes[j].nodeName] = nodes[i].firstChild.childNodes[j].firstChild.nodeValue;
					}
					var resultType = nodes[i].nodeName;
					switch(resultType) {
						case "entry":
							metadataAr.push(metadata);
							var dataxml = (nodes[i].childNodes[1].firstChild.nodeValue); // as CDATA
							if(dataxml == null) dataxml = (nodes[i].childNodes[1].firstChild); // as XML
							data = new XML(dataxml);
							if(dataxml == undefined) break
							if(data.firstChild.nodeName == null) responseAr.push(getDepotObject(data.firstChild, true));
							else responseAr.push(getDepotObject(data.firstChild));
						break;
						case "directory":
						case "sub-directory":
							metadata["content-type"] = resultType;
							directoryAr.push(metadata)
						break;
						default:
						trace("Error in 200 resultType")
						break;
					}
				}
				_onGet(responseAr, metadataAr, directoryAr);
				break;
			case (201): // Successful PUT request 
				if(response.getHeader("X-Object-Id")) {
					if(_onCreate) _onCreate(response.getHeader("X-Object-Id"));
				} else {
					if(_onCreateDirectory) _onCreateDirectory(response.getHeader("Data"));
				}
				break;
			case (202): // Accepted 	
			case (203): // Non-Authoritative Information 	
			case (204): // No Content 	
			case (205): // Reset Content 	
			case (206): // Partial Content 
			case (223): // Succesful delete
				_onDelete(response.getHeader("Data"));
				break;				
			case (Code<300): // SUCCESS (not supported)
			case (300): // Multiple Choices 
			case (301): // Moved Permanently
			case (302): // Moved Temporarily (HTTP/1.0) 
			case (303): // See Other (HTTP/1.1) 
			case (304): // Not Modified 
			case (305): // Use Proxy
			case (306): // (unused, but reserved) 
			case (307): // Temporary Redirect 
			case (Code<400): // REDIRECTION
			case (400): // Bad Request malformed request, accompanied by parse error 
			case (401): {
				_authenticationHeader.setHeaders(response.getHeader("WWW-Authenticate"));
				pError(genericMessage, Code, _authenticationHeader, _instance);
				break;
				}// Unauthorized TBD 
			case (402): // Payment Required 
			case (403): // Forbidden TBD 
				pError(genericMessage, Code, _authenticationHeader, _instance);
				break;
			case (404): // Not Found Non-existant resource 
			case (405): // Method Not Allowed 
			case (406): // Not Acceptable 
			case (407): // Proxy Authentication Required 
			case (408): // Request Timeout Client couldn’t connect to Depot server 
			case (409): // Conflict attempting to create a table that allready exists	
			case (410): // Gone 	
			case (411): // Length Required TBD 
			case (412): // Precondition Failed 
			case (413): // Request Entity Too Large 
			case (414): // Request-URI Too Long
			case (415): // Unsupported Media Type
			case (416): // Requested Range Not Satisfiable 
			case (417): // Expectation Failed
			case (Code<500): // CLIENT ERROR
			case (500): // Internal Server Error General error, usually means a Depot bug 
			case (501): // Not Implemented A request type or query syntax is correct but is not yet implemented by the Depot server  
			case (502): // Bad Gateway 
			case (503): // Service Unavailable
			case (504): // Gateway Timeout 
			case (505): // HTTP Version Not Supported 
			case (509): // Bandwidth Limit Exceeded 
			case (Code<600): // SERVER ERROR
			default:
				pError(genericMessage, Code);
				break;
		}	
	};
	private function send(req):Void {
		_stack.push(req);
		if(_connectionState == false) connect();
	}
	private function connect(Void):Void {
		clearInterval(_int);
		_connectionState = true;
		_int = setInterval(this,"destroySocket", timeout);
		_sock.connect(_ip,_port);
	}
	private function destroySocket() {
		clearInterval(_int)
		pError("Socket timeout", 408);
		_sock = null;
	}
	
	//// GETTERS/SETTERS

	public function get connectionState():Boolean {
		return _connectionState;
	}
	public function set onConnect(func:Function):Void {
		_onConnect = func;
	}
	public function set onError(func:Function):Void {
		_onError = func;
	}
	public function set onGet(func:Function):Void {
		_onGet = func;
	}
	public function set onCreate(func:Function):Void {
		_onCreate = func;
	}
	public function set onCreateDirectory(func:Function):Void {
		_onCreateDirectory = func;
	}
	public function set onDelete(func:Function):Void {
		_onDelete= func;
	}
	public function set debug(bool:Boolean):Void {
		_debug= bool;
	}
	public function set tracepath(tgt):Void {
		_tracepath = tgt;
	}
}
