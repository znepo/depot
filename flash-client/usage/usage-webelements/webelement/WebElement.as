/* 
//
//	FILE:			WebElement.as
//
//	DESCRIPTION:	Depot Web Element Usage Example
//
//	REVISION:		23 08 2006	v0.1 alpha
//
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
//
*/

//// IMPORTS

import mx.utils.Delegate;
import mx.events.EventDispatcher;
import com.snepo.depot.*;

class webelement.WebElement extends MovieClip
{
	
	public function addEventListener(){};
	public function removeEventListener(){};
	public function dispatchEvent(){};
	
	public var elementType:Object;
	public var DP:Depot;
	public var $url:String;
	
	public var elementLoader:MovieClipLoader;
	public var element:MovieClip;
	
	public function WebElement ()
	{
		EventDispatcher.initialize ( this );
		element = this.createEmptyMovieClip("element", 0 );
	}
	
	public function initialize ( ip:String, port:Number, __url:String, elemPath:String )
	{
		DP = new Depot ( ip, port );
		$url = __url;
		elementLoader = new MovieClipLoader();
		elementLoader.loadClip(elemPath, this.element);
		elementLoader.addListener ( this );	
	}
	
	public function onLoadInit ( e:MovieClip )
	{
		e.setDepotInstance ( DP );
	}
	
	
	
}