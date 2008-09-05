//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.aligner
import xm.corpus.alignResizeReceiver;

class xm.corpus.alignData {

	// Position

	var hAlign:Boolean = false;
	var vAlign:Boolean = false;

	var hPosition:Number = 0;
	var vPosition:Number = 0;
	var hPositionMargin:Number = 0;
	var vPositionMargin:Number = 0;
	var hPositionOffset:Number = 0;
	var vPositionOffset:Number = 0;

	var hCenterPointCalculated:String = "none";
	var vCenterPointCalculated:String = "none";

	var hPositionRounding:Function = noRounding;
	var vPositionRounding:Function = noRounding;

	var resizeReceiver:alignResizeReceiver = undefined;

	// Size

	var hStretch:Boolean = false;
	var vStretch:Boolean = false;

	var hSize:Number = 1;
	var vSize:Number = 1;
	var hSizeMargin:Number = 0;
	var vSizeMargin:Number = 0;

	var hSizeRounding:Function = noRounding;
	var vSizeRounding:Function = noRounding;

	// Size (proportional)

	var proportionalStretch:String = "none";
	var boundingRectagleWidth:Number = 550;
	var boundingRectagleHeight:Number = 400;

	function alignData() {
	}

	// Util

	static function noRounding(p_n:Number):Number {
		return p_n;
	}

}