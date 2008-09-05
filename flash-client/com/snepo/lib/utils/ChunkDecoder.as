/* 
//
//	FILE:				Serializer.as 
//
//	DESCRIPTION:		Decodes chuked data returned frm Apache proxy
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS
	
//import com.snepo.lib.iso.ISO;
//import com.snepo.lib.utils.Utils;

class com.snepo.lib.utils.ChunkDecoder {
	
	//// UTILITY METHODS
	
	private var chunkedString:String;
	private var unChunkedString:String;
	
	public function decode(str) {
		chunkedString = str;
		unChunkedString = "";
		while (chunkedString.length>4) {
			var len = getLen(chunkedString);
			var st = chunkedString.indexOf("\r\n")+2;
			var end = chunkedString.indexOf("\r\n", len);
			unChunkedString += chunkedString.substr(st, len);
			chunkedString = chunkedString.substr(end+2);
		}
		return unChunkedString.substr(0, unChunkedString.length);
	}
	
	private function getLen(str:String):Number {
		return Number("0x"+str.split("\r\n")[0]);
	}
	
}
