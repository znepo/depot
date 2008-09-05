//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

// ScrollbarPaneButtonComponent

import xm.components.SimplePaneComponent;

class xm.components.SBPButtonComponent extends SimplePaneComponent {

	[Event("start")]
	[Event("stop")]

	function SBPButtonComponent() {
	}

	function onRelease(Void):Void {
		dispatchEvent({type: "stop"});
		this.gotoAndStop(3);
	}

	function onRollOver(Void):Void {
		this.gotoAndStop(3);
	}

	function onPress(Void):Void {
		dispatchEvent({type: "start"});
		this.gotoAndStop(4);
	}

	function onReleaseOutside(Void):Void {
		dispatchEvent({type: "stop"});
		this.gotoAndStop(2);
	}

	function onRollOut(Void):Void {
		this.gotoAndStop(2);
	}

}
