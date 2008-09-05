//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.resizableComponent;
import xm.corpus.textFieldHelper;

class xm.components.VScrollbarComponent extends resizableComponent {

	[IconFile("VScrollbarComponent.png")]

	static var tfHelper:textFieldHelper = new textFieldHelper();

	[Inspectable(name="target")]
	var _targetInstanceName:MovieClip;
	var target:MovieClip;

	var _flipable:Boolean = true;

	private var __enabled:Boolean;
	private var __horizontal:Boolean;

	function VScrollbarComponent() {
	}

	function get horizontal():Boolean {
		return __horizontal;
	}

	function set horizontal(p_val:Boolean):Void {
		__horizontal = p_val
		if (!update) {
			return;
		}
		if (p_val) {
			_bs = "xrightScroll";
			_s = "xhscroll";
			_ms = "xmaxhscroll";
		}
		else {
			_bs = "bottomScroll";
			_s = "scroll";
			_ms = "maxscroll";
		}
		if (!p_val && _orientation) {
			this._xscale = _orotation ? 100 : -100;
			this._rotation = _orotation ? 0 : 180;
		}
		else if (p_val && (_orientation == false || _orientation == undefined && _orotation)) {
			this._xscale = _orotation ? -100 : 100;
			this._rotation = _orotation ? -90 : 90;
		} else if (p_val && !_orientation && !_orotation) {
			setSize(height, width);
		}

		update();
		_orientation = p_val;
	}

  	[Inspectable(defaultValue=true)]
	function get enabled():Boolean {
		return __enabled;
	}

	function set enabled(p_val:Boolean):Void {
		__enabled = p_val;
		update();
	}

	private var _orientation:Boolean;
	private var _orotation:Boolean;

	var hStep:Number = 10;
	var vStep:Number = 1;

	private var _step:Number;

	private var _bs:String;
	private var _s:String;
	private var _ms:String;

	private var sliderPos:Number;
	private var sliderMargin:Number;

	private var _duinterval:Number;
	private var _sinterval:Number;

	var update:Function;

	var slider:MovieClip;

	function _update(Void):Void {

		if (!enabled || !target[_ms] || target[_ms] < 2 || _s == "xhscroll" && target.textWidth + 5 <= target._width) {
			if (_s == "xhscroll" && target.xhscroll <= 1 && enabled) {
				target.xhscroll = 1;
			}
			if (_currentframe != 3) {
				gotoAndStop(3);
			}
		}
		else {

			if (_currentframe != 2) {
				gotoAndStop(2);
			}

			var h = Math.round((target[_bs] - target[_s] + 1)/(target[_bs] - target[_s] + target[_ms])*(height-sliderMargin))

			if (slider.setSize) {
				slider.setSize(slider.width, h);
			}
			else {
				slider._height = h;
			}

			slider._y = sliderPos + Math.round((height-sliderMargin - (slider.height ? slider.height : slider._height)) * (Math.min(1, (target[_s]-1)/(target[_ms]-1))));

		}

	}

	function delayedUpdate(Void):Void {
		onEnterFrame = nextFrameUpdate;
		clearInterval(_duinterval);
		delete _duinterval;
	}

	function nextFrameUpdate(Void):Void {
		update();
		delete onEnterFrame;
	}

	function onScroller(Void):Void {
		if (_duinterval) {
			clearInterval(_duinterval);
		}
		_duinterval = setInterval(this, "delayedUpdate", 100);
	}

	function onUnload(Void):Void {
		clearInterval(_duinterval);
		clearInterval(_sinterval);
	}

	function doScroll(p_pos:Number):Void {
		target[_s] = Math.min(target[_ms], Math.max(1, p_pos));
	}

	function _onSliderMove(Void):Void {
		updateAfterEvent();
		doScroll(Math.round((slider._y - sliderPos)/(height-sliderMargin-(slider.height ? slider.height : slider._height))*(target[_ms]-1)+1));
	}

	function startSliderDrag(Void):Void {
		target.removeListener(this);
		onMouseMove = _onSliderMove;
		slider.startDrag(false, slider._x, sliderPos, slider._x, sliderPos+height-sliderMargin-(slider.height ? slider.height : slider._height));
	}

	function stopSliderDrag(Void):Void {
		target.addListener(this);
		delete onMouseMove;
		slider.stopDrag();
		update();
	}

	function _doScrolling(p_first:Boolean):Void {
		if (p_first) {
			clearInterval(_sinterval);
			_sinterval = setInterval(this, "_doScrolling", 100);
		}
		doScroll(target[_s] + _step);
	}

	function startScrolling(p_type:String):Void {
		if (p_type == "page") {
			_step = (target[_bs]-target[_s]) * (slider._ymouse >= 0 ? 1 : -1);
		}
		else {
			_step = (p_type == "up" ? -1 : 1) * (horizontal ? hStep : vStep);
		}
		_doScrolling();
		_sinterval = setInterval(this, "_doScrolling", 500, true);
	}

	function stopScrolling(Void):Void {
		clearInterval(_sinterval);
		delete _sinterval;
	}

	function setSize(p_width:Number, p_height:Number):Void {
		super.setSize(p_width, p_height);
		update();
	}

	function init(Void):Void {

		_orotation = _rotation == 0;

		super.init();
		nextFrame();

		if (_name == "_hs") {
			__enabled = _parent.__enabled;
			__width = _parent.width;
			__height = _parent.height;
			__horizontal = true;
		}

		sliderMargin = horizontal ? _width - slider._height : _height - slider._height;
		sliderPos = slider._y;

		target = _parent[_targetInstanceName];

		update = _update;

		// delayed initalization
		horizontal = horizontal;

		target.addListener(this);
		tabChildren = false;

	}

}