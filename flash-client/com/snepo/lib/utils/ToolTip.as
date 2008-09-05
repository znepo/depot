/* 
//
//	FILE:				ToolTip.as 
//
//	DESCRIPTION:		Utility for UI tool tips (written by Andrew Wright)
//
//	REVISION:			2006 09 18	v1.2.1 alpha	
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

class com.snepo.lib.utils.ToolTip extends MovieClip
{
	private var TipText:TextField;
	private var Backing:MovieClip;
	private var Inner:MovieClip;
	
	private var xLocation:Number;
	private var yLocation:Number;
	
	private var wSize:Number;
	private var hSize:Number;
	private var ta:Number = 0;
	
	private var delayctr:Number = 0;
	private var delay:Number =30;
	
	public function ToolTip()
	{
		TipText = Inner.TipText;
		Backing = Inner.Backing;
		TipText.autoSize = "left";
	}
	
	public function setSize(w:Number, h:Number)
	{
		Backing._width = w;
		Backing._height = h;
		
		wSize = w;
		hSize = h;
	}
	
	public function getSize():Object
	{
		return {w:wSize, h:hSize};
	}
	
	public function setLocation(x:Number, y:Number)
	{
		xLocation = x;
		yLocation = y;
		
		this._x = x;
		this._y = y;
	}
	
	public function getLocation():Object
	{
		return {x:xLocation, y:yLocation};
	}
	
	public function setLabel(aLabel:String)
	{
		TipText.htmlText = aLabel.toUpperCase();
		setSize(TipText.textWidth + 8, TipText.textHeight + 2);
	}
	
	public function getLabel():String
	{
		return TipText.htmlText;
	}
	
	public function setColor(aColor:String)
	{
		var col = new Color(Backing);
			col.setRGB(aColor);
	}
	
	public function getColor():String
	{
		var col = new Color(Backing);
		return "0x" + col.getRGB().toString(16);
	}
	
	public function activate(aLabel:String)
	{
		this.swapDepths(this._parent.getNextHighestDepth());
		setLabel ( aLabel );
		this.delayctr = 0;
		
		this.onEnterFrame = function()
		{
			if ( this.delayctr++ > this.delay )
			{
				this.ta = 100;
			}
			
			this._alpha += (this.ta-this._alpha)/10;
			this._x = this._parent._xmouse;
			this._y = this._parent._ymouse;
			if ( this._x + this._width + 15 > Stage.width )
			{
				Inner._x = -this._width-5;
			}else
			{
				Inner._x = 15;
			}
			
		}
		this._visible = true;
		
	}
	
	public function deactivate()
	{
		this.delayctr = 0;
		this._alpha = 0;
		this.ta = 0;
		delete this.onEnterFrame;
		this._visible = false;
	}
}
