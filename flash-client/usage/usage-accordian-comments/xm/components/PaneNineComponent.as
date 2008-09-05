//
// Copyright 2005 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.resizableComponent;

class xm.components.PaneNineComponent extends resizableComponent {
	
	var vSizeMargin:Number = 10;
	var hSizeMargin:Number = 10;
	
	var tl, tc, tr, ml, mc, mr, bl, bc, br: MovieClip;
	
	function PaneNineComponent() {
	}
	
	function registerPaneElements(Void):Void {
		
		Aligner.register(tc, {hStretch: true, hSizeMargin: hSizeMargin});
		Aligner.register(tr, {hAlign: true, hPosition: 1});
		Aligner.register(ml, {vStretch: true, vSizeMargin: vSizeMargin});
		Aligner.register(mc, {hStretch: true, hSizeMargin: hSizeMargin, vStretch: true, vSizeMargin: vSizeMargin});
		Aligner.register(mr, {hAlign: true, hPosition: 1, vStretch: true, vSizeMargin: vSizeMargin});
		Aligner.register(bl, {vAlign: true, vPosition: 1});
		Aligner.register(bc, {vAlign: true, vPosition: 1, hStretch: true, hSizeMargin: hSizeMargin});
		Aligner.register(br, {hAlign: true, hPosition: 1, vAlign: true, vPosition: 1});
		
	}

	function init(Void):Void {
		
		super.init();
		nextFrame();
		
		registerPaneElements();
		
	}

}
