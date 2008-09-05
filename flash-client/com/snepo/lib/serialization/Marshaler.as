/* 
//
//	FILE:				Marshaler.as 
//
//	DESCRIPTION:		Marshals serialised strings into the required data type
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS
	
import com.snepo.lib.iso.ISO;

class com.snepo.lib.serialization.Marshaler {
	
	//// UTILITY METHODS
	
	static function marshal(response:XML, objType:String) {
		switch(objType) {
			case "xml":
				var tempXML = new XML(response.toString());
				return tempXML;
				break;
			case "string":
				return response.toString();
				break;
			case "number":
				return parseFloat(response.toString());
				break;
			case "boolean":
				return (response.toString() == "true")? true:false;
				break;
			case "date":
				return ISO.stringToDate(response.toString());
				break;
			default:
				trace("Depot Error: unsupported object type (serial). "+objType); // TODO: error
				return;
				break;
		}		
	}
}