//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.aligner;

class xm.corpus.alignResizeReceiver {

	var alignee:String;
	var alignerObj:aligner;

	function alignResizeReceiver(p_clip:String, p_alignerObj:aligner) {
		alignee = p_clip;
		alignerObj = p_alignerObj;
	}

	function resize(Void):Void {
		alignerObj.update(alignee);
	}

}