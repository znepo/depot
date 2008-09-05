//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.aligner;
import xm.corpus.alignData;

class xm.components.AlignComponent extends MovieClip {

	[IconFile("AlignComponent.png")]

	[Inspectable(name="target")]
	var _targetInstanceName:MovieClip;
	[Inspectable(name="host")]
	var host:String = "gStage";

	[Inspectable(name="hAlign")]
	var hAlign:Boolean = false;

		[Inspectable(name="   hPosition")]
		var hPosition:Number = 0;
		[Inspectable(name="   hPositionOffset")]
		var hPositionOffset:Number = 0;
		[Inspectable(name="   hPositionMargin")]
		var hPositionMargin:Number = 0;
		[Inspectable(name="   hCenterPointCalculated", enumeration="none,center,right")]
		var hCenterPointCalculated:String = "none";
		[Inspectable(name="   hPositionRounding", enumeration="noRounding,floor,round,ceil")]
		var hPositionRounding:String = "noRounding";

	[Inspectable(name="vAlign")]
	var vAlign:Boolean = false;

		[Inspectable(name="   vPosition")]
		var vPosition:Number = 0;
		[Inspectable(name="   vPositionMargin")]
		var vPositionMargin:Number = 0;
		[Inspectable(name="   vPositionOffset")]
		var vPositionOffset:Number = 0;
		[Inspectable(name="   vCenterPointCalculated", enumeration="none,center,bottom")]
		var vCenterPointCalculated:String = "none";
		[Inspectable(name="   vPositionRounding", enumeration="noRounding,floor,round,ceil")]
		var vPositionRounding:String = "noRounding";

	[Inspectable(name="hStretch")]
	var hStretch:Boolean = false;

		[Inspectable(name="   hSize")]
		var hSize:Number = 1;
		[Inspectable(name="   hSizeMargin")]
		var hSizeMargin:Number = 0;
		[Inspectable(name="   hSizeRounding", enumeration="noRounding,floor,round,ceil")]
		var hSizeRounding:String = "noRounding";

	[Inspectable(name="vStretch")]
	var vStretch:Boolean = false;

		[Inspectable(name="   vSize")]
		var vSize:Number = 1;
		[Inspectable(name="   vSizeMargin")]
		var vSizeMargin:Number = 0;
		[Inspectable(name="   vSizeRounding", enumeration="noRounding,floor,round,ceil")]
		var vSizeRounding:String = "noRounding";

	// Size (proportional)

	[Inspectable(name="proportionalStretch", enumeration="none,in,out")]
	var proportionalStretch:String = "none";

		[Inspectable(name="   boundingRectagleWidth")]
		var boundingRectagleWidth:Number = 550;
		[Inspectable(name="   boundingRectagleHeight")]
		var boundingRectagleHeight:Number = 400;

	private var _hostEval:aligner;

	function AlignComponent() {
		init();
	}

	function init(Void):Void {

		_targetInstanceName = _parent[_targetInstanceName];

		var instance:MovieClip = _targetInstanceName;
		var lHost:String = host;

		delete _targetInstanceName;
		delete host;

		if (lHost == "" || lHost == undefined) {
			lHost = "Stage";
		}
		else if (lHost == "this") {
			lHost = String(_parent);
		}

		tellTarget(_parent) {
			if (typeof eval(lHost).Aligner != "object") {
				this._hostEval = eval(lHost).Aligner = new aligner(eval(lHost));
			}
			else {
				this._hostEval = eval(lHost).Aligner;
			}
		}

		var hostEval:aligner = _hostEval;
		delete _hostEval;

		Object(this).hPositionRounding = hPositionRounding != "noRounding" ? Math[hPositionRounding] : alignData.noRounding;
		Object(this).vPositionRounding = vPositionRounding != "noRounding" ? Math[vPositionRounding] : alignData.noRounding;
		Object(this).hSizeRounding = hSizeRounding != "noRounding" ? Math[hSizeRounding] : alignData.noRounding;
		Object(this).vSizeRounding = vSizeRounding != "noRounding" ? Math[vSizeRounding] : alignData.noRounding;

		hostEval._register(instance, this);

		_hostEval = hostEval;
		_targetInstanceName = instance;

		onEnterFrame = function () {
			_hostEval.alignees[_targetInstanceName]._allowRemove = true;
			delete onEnterFrame;
		}

		onUnload = function () {
			_hostEval._remove(String(_targetInstanceName));
		}

		nextFrame();

	}

}