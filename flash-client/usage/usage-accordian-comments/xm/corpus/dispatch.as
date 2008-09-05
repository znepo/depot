//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

class xm.corpus.dispatch {

	static var dispatchInstance:dispatch = new dispatch();
	private var _$listeners;

	static function initialize(p_obj:Object):Void {

		p_obj.addEventListener = dispatchInstance.addEventListener;
		p_obj.removeEventListener = dispatchInstance.removeEventListener;
		p_obj.addEventListenerL = dispatchInstance.addEventListenerL;
		p_obj.removeEventListenerL = dispatchInstance.removeEventListenerL;
		p_obj.removeAllEventListeners = dispatchInstance.removeAllEventListeners;
		p_obj.removeAllListeners = dispatchInstance.removeAllListeners;
		p_obj.dispatchEvent = dispatchInstance.dispatchEvent;

		p_obj._$listeners = {};

	}

	function dispatch() {
	}

	private function dispatchEvent(p_eventObj:Object):Void {

		var eventName:String = p_eventObj.type;
		var eLst:Array = _$listeners[eventName];

		this[eventName+"Handler"](p_eventObj);

		var i:Number;
		for (i in eLst) {

			var hType:String = typeof eLst[i].f;

			if (hType == "string") {
				eLst[i].o[eLst[i].f](p_eventObj);
			}
			else if (hType == "function") {
				eLst[i].f.apply(eLst[i].o, p_eventObj);
			}
			else if (hType == "object" || hType == "movieclip") {
				if (eLst[i].f.handleEvent) {
					eLst[i].f[eventName](p_eventObj);
				}
				else {
					eLst[i].f.handleEvent(p_eventObj);
				}
			}

		}

		if (eventName != "ALL") {
			p_eventObj.type = "ALL";
			dispatchEvent(p_eventObj);
		}

	}

	private function addEventListener(p_eventName:String, p_handler):Void {
		if (typeof p_handler == "function") {
			addEventListenerL(p_eventName, this, p_handler);
		}
		else {
			addEventListenerL(p_eventName, p_handler);
		}
	}

	private function addEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {

		var eLst:Array = _$listeners[p_eventName];

		if (p_handler == undefined) {
			p_handler = p_eventName;
		}

		if (eLst == undefined) {
			var eLst = _$listeners[p_eventName] = [];
		}

		var i:Number;
		for (i in eLst) {
			if (eLst[i].o == p_obj && eLst[i].f == p_handler) {
				return;
			}
		}

		eLst[eLst.length] = {o: p_obj, f: p_handler};

	}

	private function removeEventListener(p_eventName:String, p_handler):Void {
		if (typeof p_handler == "function") {
			removeEventListenerL(p_eventName, this, p_handler);
		}
		else {
			removeEventListenerL(p_eventName, p_handler);
		}
	}

	private function removeEventListenerL(p_eventName:String, p_obj:Object, p_handler):Void {

		var eLst:Array = _$listeners[p_eventName];

		var i:Number;
		for (i in eLst) {
			if (eLst[i].o == p_obj && eLst[i].f == p_handler) {
				eLst.splice(i, 1);
				return;
			}
		}

	}

	private function removeAllEventListeners(p_eventName:String):Void {
		delete _$listeners[p_eventName];
	}

	private function removeAllListeners(Void):Void {
		_$listeners = {};
	}

}