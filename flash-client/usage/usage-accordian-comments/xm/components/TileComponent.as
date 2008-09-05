//
// Copyright 2004 Elvis Mehmedovic.
// http://emehmedovic.com/
//

import xm.corpus.resizableComponent;

class xm.components.TileComponent extends resizableComponent {

	[IconFile("TileComponent.png")]

	[Event("rowCountChange")]
	[Event("colCountChange")]

	private var __tile:String;
	private var __manualTileWidth:Number;
	private var __manualTileHeight:Number;
	private var __tileSpacingH:Number;
	private var __tileSpacingV:Number;
	private var __applyMask:Boolean;

	var rowCount:Number = 0;
	var colCount:Number = 0;

	private var _notile = true;

	private var _tileWidth:Number;
	private var _tileHeight:Number;

	private var _c:MovieClip;
	private var _m:MovieClip;

	private var refresh:Function;

	function TileComponent() {
	}

	function init(Void):Void {

		super.init();

		createEmptyMovieClip("_c", 1);

		applyMask = applyMask;

		refresh = _refresh;
		refresh();

	}

	function setSize(p_width:Number, p_height:Number):Void {

		super.setSize(p_width, p_height);
		_m._width = p_width;
		_m._height = p_height;

		refresh();

	}

  	[Inspectable(defaultValue=0)]
	function get manualTileHeight():Number {
		return __manualTileHeight;
	}

	function set manualTileHeight(p_val:Number) {
		flush();
		__manualTileHeight = p_val;
		refresh();
	}

  	[Inspectable(defaultValue=0)]
	function get manualTileWidth():Number {
		return __manualTileWidth;
	}

	function set manualTileWidth(p_val:Number) {
		flush();
		__manualTileWidth = p_val;
		refresh();
	}

  	[Inspectable(defaultValue=0)]
	function get tileSpacingH():Number {
		return __tileSpacingH;
	}

	function set tileSpacingH(p_val:Number) {
		flush();
		__tileSpacingH = p_val;;
		refresh();
	}

  	[Inspectable(defaultValue=0)]
	function get tileSpacingV():Number {
		return __tileSpacingV;
	}

	function set tileSpacingV(p_val:Number) {
		flush();
		__tileSpacingV = p_val;;
		refresh();
	}

	private function flush(Void):Void {

		for (var i:String in _c) {
			_c[i].removeMovieClip();
		}

		// change

		rowCount = 0;
		colCount = 0;

	}

  	[Inspectable(defaultValue=true)]

	function get applyMask():Boolean {
		return __applyMask;
	}

	function set applyMask(p_val:Boolean) {

		__applyMask = p_val;

		if (!_c) {
			return;
		}

		if (p_val) {
			gotoAndStop(1);
			_m._width = width;
			_m._height = height;
			_c.setMask(_m);
		}
		else {
			gotoAndStop(2);
			_c.setMask(null);
		}

	}

	function disableRefresh(p_val:Boolean):Void {
		if (p_val) {
			delete refresh;
		}
		else {
			refresh = _refresh;
			refresh();
		}
	}

	[Inspectable(name="tile")]

	function get tile():String {
		return __tile;
	}

	function set tile(p_val:String) {

		flush();

		__tile = p_val;

		var mc:MovieClip = attachMovie(p_val, null, 1);

		_notile = !mc._width || !mc._height;

		_tileWidth = mc._width;
		_tileHeight = mc._height;

		mc.removeMovieClip();

		refresh();

	}


	function _refresh(Void):Void {

		if (_notile) {
			return;
		}

		var oRowCount:Number = rowCount;
		var oColCount:Number = colCount;

		if (manualTileWidth) {
			var tileWidth:Number = manualTileWidth + tileSpacingH;
			var tileHeight:Number = manualTileHeight + tileSpacingV;
		}
		else {
			var tileWidth:Number = _tileWidth + tileSpacingH;
			var tileHeight:Number = _tileHeight + tileSpacingV;
		}

		rowCount = Math.ceil(height/tileHeight);
		colCount = Math.ceil(width/tileWidth);

		if (oColCount != colCount || oRowCount != rowCount) {
			if (oColCount != colCount) {
				dispatchEvent({type: "colCountChange"});
			}

			if (oRowCount != rowCount) {
				dispatchEvent({type: "rowCountChange"});
			}
		}
		else {
			return;
		}

		for (var i = colCount + 1; i <= oColCount; i++) {
			for (var j = 1; j <= oRowCount; j++) {
				_c[i+"_"+j].removeMovieClip();
			}
		}

		for (var j = rowCount + 1; j <= oRowCount; j++) {
			for (var i = 1; i <= oColCount; i++) {
				_c[i+"_"+j].removeMovieClip();
			}
		}

		for (var i = 1; i <= colCount; i++) {
			for (var j = 1; j <= rowCount; j++) {
				if (i > oColCount || j > oRowCount) {
					var t = _c.attachMovie(tile, i+"_"+j, i + j*1028, {row: j, col: i, tile: this});
					t._x = (i-1) * tileWidth;
					t._y = (j-1) * tileHeight;
				}
			}
		}

	}

}