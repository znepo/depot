//
// Copyright 2004, 2005 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.dispatch;
import xm.corpus.aligner;

class xm.corpus.resizableComponent extends MovieClip {

	[Event("resize")]

	var Aligner:aligner;
	
	var minWidth:Number = 0;
	var minHeight:Number = 0;
	var maxWidth:Number = Number.POSITIVE_INFINITY;
	var maxHeight:Number = Number.POSITIVE_INFINITY;

	private var __width:Number;
	private var __height:Number;

	function get width():Number {
		return __width;
	}

	function get height():Number {
		return __height;
	}

	function setSize(p_width:Number, p_height:Number):Void {
		p_width = Math.min(maxWidth, Math.max(minWidth, p_width));
		p_height = Math.min(maxHeight, Math.max(minHeight, p_height));
		if (__width != p_width || __height != p_height) {
			__width = p_width;
			__height = p_height;
			dispatchEvent({type: "resize"});
		}
	}

	function resizableComponent() {
		init();
	}

	function init(Void):Void {

		dispatch.initialize(this);

		if (width == undefined) {
			__width = Math.min(maxWidth, Math.max(minWidth, _width));
		}
		if (height == undefined) {
			__height = Math.min(maxHeight, Math.max(minHeight, _height));
		}
		
		_xscale = 100;
		_yscale = 100;
		
		Aligner = new aligner(this);

	}

	// dispatch
		function dispatchEvent(p_eventObj:Object):Void {}
		function addEventListener(p_eventName:String, p_handler):Void {}
		function addEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {}
		function removeEventListener(p_eventName:String, p_handler):Void {}
		function removeEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {}
		function removeAllEventListeners(p_eventName:String):Void {}

}