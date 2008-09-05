//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.resizableComponent;
import xm.components.VScrollbarComponent;

class xm.components.HScrollbarComponent extends resizableComponent {

	[IconFile("HScrollbarComponent.png")]

	var _hs:VScrollbarComponent;

	[Inspectable(name="target")]
	var _targetInstanceName:MovieClip;

	function HScrollbarComponent() {
	}

  	[Inspectable(defaultValue=true)]
	function get enabled():Boolean {
		return _hs.enabled;
	}

	function set enabled(p_val:Boolean):Void {
		_hs.enabled = __enabled = p_val;
	}

	private var __enabled:Boolean;

	function init(Void):Void {
		super.init();
		_targetInstanceName = _parent[_targetInstanceName];
		nextFrame();
	}

	function setSize(p_width:Number, p_height:Number):Void {
		super.setSize(p_width, p_height);
		if (_hs.width != height || _hs.height != height) {
			_hs.setSize(height, width);
		}
	}

}