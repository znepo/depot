//
// Copyright 2005 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.components.GenericScrollbarClient;

class xm.components.ScrollAreaComponent extends GenericScrollbarClient {
	
	[IconFile("ScrollAreaComponent.png")]
	
	var content:MovieClip;
	var mcLoader:MovieClipLoader;
	
	var manualWidth:Number;
	var manualHeight:Number;
	
	var mouseWheelHStep:Number = 10;
	var mouseWheelVStep:Number = 10;
	
	var easingFunction:Function = easeOut;
	
	[Inspectable(defaultValue=true)]
	var easing:Boolean;
	
	[Inspectable(defaultValue=15)]
	var easingDuration:Number;
	
	private var _m:MovieClip;
	
	private var $fix:Number;
	private var $sfix:Number;
	
	private var __easing:Boolean;
	private var __mouseWheel:String;
	private var __applyMask:Boolean;
	private var __symbol:String;
	private var __url:String;
	
	private var _t:Number;
	
	private var gotoX:Number = 0;
	private var gotoY:Number = 0;
	private var oldX:Number;
	private var oldY:Number;
	private var lcontent:MovieClip;
	
	
	[Inspectable()]
	function get url():String {
		return __url;
	}

	function set url(p_val:String) {
		
		__url = p_val;
		
		if (width == undefined) {
			return;
		}
		
		$sfix = $sfix > 1000 || !$sfix ? 1 : $sfix + 1;
		
		lcontent.removeMovieClip();
		lcontent = createEmptyMovieClip("lcontent"+$sfix, getNextHighestDepth());
		
		mcLoader.loadClip(p_val, lcontent);
		
	}
	
	private function onLoadInit(Void):Void {
		
		//onEnterFrame = _update;
		delete onEnterFrame;
		
		gotoX = 0;
		gotoY = 0;
		
		content.removeMovieClip();
		content = lcontent;
		delete lcontent;
		
		applyMask = applyMask;
		
		update();
		
	}
	
	private function initiateTween(Void):Void {
		
		oldX = content._x;
		oldY = content._y;
		
		_t = 0;
		
		onEnterFrame = _onEnterFrame;
		onEnterFrame();
		
	}
	
	private function _update(Void):Void {
		
		update();
		delete onEnterFrame;
		
	}
	
	private function _onEnterFrame(Void):Void {
		
		_t++;
		
		content._x = Math.round(easingFunction(_t, oldX, gotoX - oldX, easingDuration));
		content._y = Math.round(easingFunction(_t, oldY, gotoY - oldY, easingDuration));
		
		if (_t >= easingDuration) {
			delete onEnterFrame;
		}
		
	}
	
	[Inspectable(defaultValue="auto", enumeration="none,auto,horizontalScroll,verticalScroll,diagonalScroll")]
	function get mouseWheel():String {
		return __mouseWheel;
	}

	function set mouseWheel(p_val:String) {
		
		Mouse.removeListener(this);
		
		if ((__mouseWheel = p_val) != "none") {
			Mouse.addListener(this);
		}

	}
	
	[Inspectable()]
	function get symbol():String {
		return __symbol;
	}

	function set symbol(p_val:String) {
		
		__symbol = p_val;
		
		if (width == undefined) {
			return;
		}
		
		delete onEnterFrame;
		
		gotoX = 0;
		gotoY = 0;
		
		$fix = $fix > 1000 || !$fix ? 1 : $fix + 1;
		
		content.removeMovieClip();
		content = attachMovie(p_val, "content"+$fix, getNextHighestDepth(), {scrollArea: this});
		
		applyMask = applyMask;
		
		update();
		
	}
	
	private function onMouseWheel(p_delta:Number, t_scrollTarget:MovieClip):Void {
		
		if (_xmouse >= 0 && _xmouse < width && _ymouse >= 0 && _ymouse < height) {
			if (mouseWheel == "horizontalScroll") {
				xhscroll -= p_delta * mouseWheelHStep;
			}
			else if (mouseWheel == "verticalScroll") {
				scroll -= p_delta * mouseWheelVStep;
			}
			else if (mouseWheel == "diagonalScroll") {
				xhscroll -= p_delta * mouseWheelHStep;
				scroll -= p_delta * mouseWheelVStep;
			}
			else if (mouseWheel == "auto") {
				if (maxscroll == 1) {
					xhscroll -= p_delta * mouseWheelHStep;
				}
				else {
					scroll -= p_delta * mouseWheelVStep;
				}
			}
		}
		
	}
	
  	[Inspectable(defaultValue=true)]
	function get applyMask():Boolean {
		return __applyMask;
	}

	function set applyMask(p_val:Boolean) {
		
		__applyMask = p_val;
		
		if (width == undefined) {
			return;
		}
		
		if (p_val) {
			gotoAndStop(3);
			Aligner.register(_m, { hStretch: true, vStretch: true });
			content.setMask(_m);
		}
		else {
			gotoAndStop(2);
			content.setMask(null);
		}

	}
	
	function ScrollAreaComponent() {
	}
	
	function init(Void):Void {
		
		super.init();
		
		mcLoader = new MovieClipLoader();
		mcLoader.addListener(this);
		
		
		symbol = symbol;
		url = url;
	}
	
	// Override GenericScrollbarClient
	
		function get contentX():Number {
			return gotoX;
		}
		
		function set contentX(p_val:Number):Void{
			gotoX = p_val;
			if (easing) {
				initiateTween();
			} else {
				content._x = p_val;
			}
		}
		
		function get contentY():Number {
			return gotoY;
		}
		
		function set contentY(p_val:Number):Void{
			gotoY = p_val;
			if (easing) {
				initiateTween();
			} else {
				content._y = p_val;
			}
		}
		
		function get contentHeight():Number {
			if (manualHeight != undefined) {
				return Math.ceil(manualHeight);
			}
			else {
				return Math.ceil(content._height);
			}
		}
		
		function get contentWidth():Number {
			if (manualWidth != undefined) {
				return Math.ceil(manualWidth);
			}
			else {
				return Math.ceil(content._width);
			}
		}

		function addListener(p_obj:Object):Void {
			super.addListener(p_obj);
			p_obj.vStep = 10;
		}
		
	// class mx.transitions.easing.Regular
	
		static function easeOut (t:Number, b:Number, c:Number, d:Number):Number {
			return -c *(t/=d)*(t-2) + b;
		}
		
}
