//
// Copyright 2005 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.components.SimplePaneComponent;

class xm.components.GenericScrollbarClient extends SimplePaneComponent {
	
	var scrollbarClient:Boolean = true;
	
	var contentX:Number;
	var contentY:Number;
	var contentHeight:Number;
	var contentWidth:Number;
	
	function GenericScrollbarClient() {
	}
	
	function init(Void):Void {
		super.init();
	}
	
	function get scroll():Number {
		return 1 - contentY;
	}
	
	function set scroll(p_val:Number):Void {
		contentY = 1 - Math.min(maxscroll, Math.max(1, p_val));
		update();
	}
	
	function get xhscroll():Number {
		return 1 - contentX;
	}
	
	function set xhscroll(p_val:Number):Void {
		contentX = 1 - Math.min(xmaxhscroll, Math.max(1, p_val));
		dispatchEvent({type: "onScroller"});
	}
	
	function get maxscroll():Number {
		return Math.max(1, contentHeight - height + 1);
	}
	
	function get xmaxhscroll():Number {
		return Math.max(1, contentWidth - width + 1);
	}
	
	function get bottomScroll():Number {
		return scroll + height - 1;
	}
	
	function get xrightScroll():Number {
		return xhscroll + width - 1;
	}
	
	function get textWidth():Number {
		return contentWidth - 5;
	}
	
	function addListener(p_obj:Object):Void {
		addEventListenerL("onScroller", p_obj, "onScroller");
	}
	
	function removeListener(p_obj:Object):Void {
		removeEventListenerL("onScroller", p_obj, "onScroller");
	}
	
	function update(Void):Void {
		dispatchEvent({type: "onScroller"});
	}
	
}
