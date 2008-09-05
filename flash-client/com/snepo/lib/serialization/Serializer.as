/* 
//
//	FILE:				Serializer.as 
//
//	DESCRIPTION:		Serialises objects into superstrings
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS
	
import com.snepo.lib.iso.ISO;
import com.snepo.lib.utils.Utils;

class com.snepo.lib.serialization.Serializer {
	
	//// UTILITY METHODS
	
	public function serialiseObject(obj) {
		return serialise(obj)
	}
	
	static function serialise(obj):Object {
		switch(Utils.getType(obj)) {
			case "xml":
			case "string":
			case "number":
			case "boolean":
				return obj.toString();
				break;
			case "date":
				return ISO.dateToString(obj);
				break;
			case "array":
				return "["+obj.toString()+"]";
				break;
			case "object":
				var str = "{";
				for(var me in obj) {
					str += me+":"+obj[me]+", ";
				};
				str = str.substr(0,str.length-2);
				str+= "}";
				return str;
				break;
			default:
				trace("Depot Warning: unsupported object type (serialisation). "); // TODO: error
				trace(obj);
				return obj;
				break;
		}		
	}
}
