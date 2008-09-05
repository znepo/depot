//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.dispatch;
import xm.corpus.alignData;
import xm.corpus.alignResizeReceiver;

class xm.corpus.aligner {

	var redrawInertness:Number = 0;
	var host:Object;

	var resize:Function = updateAll;
	var onResize:Function = updateAll;

	var alignees:Object;

	private var _intervalid;
	private var _starttimer;

	function aligner(p_host:Object) {
		host = p_host;
		init();
	}

	function init(Void):Void {

		alignees = new Object();

		dispatch.initialize(this);

		if (host == Stage || host == _global.gStage) {
			if (Stage.scaleMode != "noScale" || Stage.align != "TL") {
				Stage.scaleMode = "noScale";	
				Stage.align = "TL";
			}
			if (Stage.showMenu) {
				Stage.showMenu = false;
			}
		}

		if (host.addEventListener) {
			host.addEventListener("resize", this);
		}
		else {
			host.addListener(this);
		}

	}

	function free(Void):Void {

		removeAll();

		if (host.addEventListener) {
			host.removeEventListener("resize", this);
		}
		else {
			host.removeListener(this);
		}

		if (_intervalid != undefined) {
			clearInterval(_intervalid);
		}

	}

	function update(p_clip:String):Void {

		var c:Object;

		if ((c = eval(p_clip)) == undefined) {
			remove(p_clip);
		}
		else {

			updateAfterEvent();

			var a:alignData = alignees[p_clip];
			var $host:Object = host;

			with (a) {

				if (hAlign || vAlign) {

					var t:Object = {};

					if (hAlign) {

						if (hCenterPointCalculated == "center") {
							t.x = hPositionOffset + hPositionRounding(($host.width - hPositionMargin) * hPosition) - Math.round((c.setSize ? c.width : c._width) / 2);
						}
						else if (hCenterPointCalculated == "right") {
							t.x = hPositionOffset + hPositionRounding(($host.width - hPositionMargin) * hPosition) - Math.round(c.setSize ? c.width : c._width);
						}
						else {
							t.x = hPositionOffset + hPositionRounding(($host.width - hPositionMargin) * hPosition);
						}

					}
					else {

						t.x = 0;

					}

					if (vAlign) {

						if (vCenterPointCalculated == "center") {
							t.y = vPositionOffset + vPositionRounding(($host.height - vPositionMargin) * vPosition) - Math.round((c.setSize ? c.height : c._height) / 2);
						}
						else if (vCenterPointCalculated == "bottom") {
							t.y = vPositionOffset + vPositionRounding(($host.height - vPositionMargin) * vPosition) - Math.round(c.setSize ? c.height : c._height);
						}
						else {
							t.y = vPositionOffset + vPositionRounding(($host.height - vPositionMargin) * vPosition);
						}

					}
					else {

						t.y = 0;

					}

					$host.localToGlobal(t);
					c._parent.globalToLocal(t);

					if (hAlign && c._x != t.x) {
						c._x = t.x;
					}
					if (vAlign && c._y != t.y) {
						c._y = t.y;
					}

				}

				if (proportionalStretch != "none") {

					if (proportionalStretch != "out" && ($host.width / $host.height >= boundingRectagleWidth / boundingRectagleHeight) || proportionalStretch == "out" && ($host.width / $host.height < boundingRectagleWidth / boundingRectagleHeight)) {
						c._xscale = ($host.height - vPositionMargin) / boundingRectagleHeight * 100;
						c._yscale = ($host.height - vPositionMargin) / boundingRectagleHeight * 100;
					}
					else {
						c._xscale = ($host.width - hPositionMargin) / boundingRectagleWidth * 100;
						c._yscale = ($host.width - hPositionMargin) / boundingRectagleWidth * 100;
					}

				}
				else {

					if (c.setSize && (hStretch || vStretch)) {
						var w:Number = hStretch ? hSizeRounding($host.width * hSize) - hSizeMargin : c.width;
						if (w < 0) {
							w = 0;
						}
						var h:Number = vStretch ? vSizeRounding($host.height * vSize) - vSizeMargin : c.height;
						if (h < 0) {
							h = 0;
						}
						if (c.width != w || c.height != h) {
							c.setSize(w, h);
						}
					}
					else {
						if (hStretch) {
							var w:Number = hSizeRounding($host.width * hSize) - hSizeMargin;
							if (w < 0) {
								w = 0;
							}
							if (c._width != w) {
								c._width = w;
							}
						}
						if (vStretch) {
							var h:Number = vSizeRounding($host.height * vSize) - vSizeMargin;
							if (h < 0) {
								h = 0;
							}
							if (c._height != h) {
								c._height = h;
							}
						}
					}

				}

			}

		}

	}

	function remove(p_clip:String):Void {

		if (alignees[p_clip].resizeReceiver != undefined) {

			var c:MovieClip = eval(p_clip);

			if (c.addEventListener) {
				c.removeEventListener("resize", alignees[p_clip].resizeReceiver);
			}
			else {
				c.removeListener(alignees[p_clip].resizeReceiver);
			}

		}

		delete alignees[p_clip];

	}

	function removeAll(Void):Void {
		var i:String;
		for (i in alignees) {
			remove(i);
		}
	}
	
	function register(p_mclip:MovieClip, p_alignData:Object):Void {
		registerOnly(p_mclip, p_alignData);
		update(String(p_mclip));
	}

	function registerOnly(p_mclip:MovieClip, p_alignData:Object):Void {

		var p_clip: String = String(p_mclip);

		if (alignees[p_clip]) {
			remove(p_clip);
		}

		var a = alignees[p_clip] = new alignData();

		var i:String;
		for (i in p_alignData) {
			a[i] = p_alignData[i];
		}

		a._clipReference = p_clip;

		if (a.hCenterPointCalculated != "none" || a.vCenterPointCalculated != "none") {

			var c:MovieClip = eval(p_clip);
			a.resizeReceiver = new alignResizeReceiver(p_clip, this);

			if (c.addEventListener) {
				c.addEventListener("resize", a.resizeReceiver);
			}
			else {
				c.addListener(a.resizeReceiver);
			}

		}

	}

	function updateAll(_timedout:Boolean):Void {

		if (_timedout == true) {
			clearInterval(_intervalid);
			_intervalid = undefined;
		}
		else if (redrawInertness > 0) {
			if (_intervalid == undefined) {
				_intervalid = setInterval(this, "updateAll", redrawInertness, true);
				_starttimer = getTimer();
				return;
			}
			else if (getTimer() - _starttimer > redrawInertness) {
				clearInterval(_intervalid);
				_intervalid = undefined;
			}
			else {
				return;
			}
		}

		var i:String;
		for (i in alignees) {
			update(i);
		}

		dispatchEvent({type: "resize"});

	}

	// component versions; fix problemes caused by late onUnload

	function _register(p_clip:MovieClip, p_alignData:Object):Void {
		p_alignData._allowRemove = false;
		register(p_clip, p_alignData);
	}

	function _remove(p_clip:String):Void {
		if (alignees[p_clip]._allowRemove) {
			remove(p_clip);
		}
	}

	// dispatch
		function dispatchEvent(p_eventObj:Object):Void {}
		function addEventListener(p_eventName:String, p_handler):Void {}
		function addEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {}
		function removeEventListener(p_eventName:String, p_handler):Void {}
		function removeEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {}
		function removeAllEventListeners(p_eventName:String):Void {}

}