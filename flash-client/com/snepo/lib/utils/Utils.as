/* 
//
//	FILE:				Utils.as
//
//	DESCRIPTION:		Utility library class
//
//	REVISION:			2006 09 18	v1.2.1 alpha
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

class com.snepo.lib.utils.Utils {
	
	//// UTILITY METHODS
	
	static function getType(obj):Object {
		if(obj instanceof Date) {
			return "date";
		} else if (obj instanceof Array) {
			return "array";
		} else if (obj instanceof XML) {
			return "xml";
		} else if (obj instanceof Object) {
			// TODO: Custom classes?
			return typeof(obj);
		} else {
			return typeof(obj);
		}
	}
}
