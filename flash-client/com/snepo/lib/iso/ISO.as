/* 
//
//	FILE:				ISO.as
//
//	DESCRIPTION:		ISO Utility Library
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

class com.snepo.lib.iso.ISO {
	
	// UTILITIES
	
	static function dateToString(d:Date):String {
		var yy = d.getUTCFullYear().toString();
		var mm = f2((d.getUTCMonth()+1).toString());
		var dd = f2(d.getUTCDate().toString());
		var hh = f2(d.getUTCHours().toString());
		var mn = f2(d.getUTCMinutes().toString());
		var ss = f2(d.getUTCSeconds().toString());
		return yy+mm+dd+hh+mn+ss;
	}
	static function stringToDate(str:String):Date {
		var yy = str.substr(0,4);
		var mm = Number(str.substr(4,2))-1;
		var dd = str.substr(6,2);
		var hh = str.substr(8,2);
		var mn = str.substr(10,2);
		var ss = str.substr(12,2);
		var d = new Date(yy,mm,dd,hh,mn,ss);
		return d;
	}
	static function f2(str:String):String {
		return (str.length <2)? "0"+str:str;
	}	
	
}