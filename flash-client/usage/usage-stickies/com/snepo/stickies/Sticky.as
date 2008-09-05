/* 
 *
 *	FILE:			Sticky.as
 *
 *	DESCRIPTION:	Sticky MovieClip class for Depot "Stickies" Usage Example
 * 
 *	PACKAGE:		com.snepo.stickies;
 *
 *	DATE CREATED:	12/09/2006;
 *
 *	AUTHOR:			Andrew Wright.
 *
 *	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
 * 
 */

import flash.filters.DropShadowFilter;

class com.snepo.stickies.Sticky extends MovieClip
{
	
	private var x:Number;
	private var y:Number;
	private var w:Number;
	private var h:Number;
	private var text:String;
	
	private var dsf:DropShadowFilter;
	
	public var stickyText:TextField;
	public var backing:MovieClip;	
	public var resizeHandles:MovieClip;
	public var dragButton:MovieClip;	
	public var closeButton:MovieClip;
	public var lockButton:MovieClip;
	public var locked:Boolean;
	public var id:Number;
	
	public function Sticky()
	{
		dsf = new DropShadowFilter(10, 45, 0x000000, 1, 5, 5, 0.2, 1);
		dragButton.onPress = function()
		{
			
			this._parent.swapDepths ( this._parent._parent.getNextHighestDepth() );
			if ( !this._parent.locked )
			{
				this._parent.startDrag ( false );
			}
		}
		
		this.filters = [dsf];
		
		dragButton.onRelease = dragButton.onReleaseOutside = function()
		{
			this._parent.stopDrag();
		}
		
		resizeHandles.onPress = function()
		{
			if ( !this._parent.locked )
			{
				this.onEnterFrame = function()
				{
					var dx:Number = (this._parent._parent._xmouse+10) - this._parent._x;
					var dy:Number = (this._parent._parent._ymouse+10) - this._parent._y;
					this._parent.setSize ( dx > 42 ? dx : 42, dy > 50 ? dy : 50 );
				}
			}
		}
		
		closeButton.onRelease = function()
		{
			this._parent._parent._parent.removeSticky ( this._parent );
		}
		
		lockButton.onPress = function()
		{
			this._parent.locked = !this._parent.locked;
			this.gotoAndStop ( Number(this._parent.locked) + 1 )
			if ( this._parent.locked )
			{
				this._parent.stickyText.type = "dynamic";
				this._parent.resizeHandles._visible = false;
			}else
			{
				this._parent.stickyText.type = "input";
				this._parent.resizeHandles._visible = true;
			}
		}
		
		resizeHandles.onRelease = resizeHandles.onReleaseOutside = function()
		{
			delete this.onEnterFrame;
		}
	}
	
	public function setID(num:Number)
	{
		this.id = num;
	}
	
	public function getID():Number
	{
		return this.id;
	}
	
	public function setSize ( w:Number, h:Number )
	{
		// if arguments[2] == true, tween the objects 
		// to their positions. otherwise just set them.
		
		if ( arguments[2] )
		{
			this.w = w;
			this.h = h;
			this.stickyText.tween("_width", w-7, 1, "easeInOutQuint" );
			this.closeButton.tween("_x", w-this.closeButton._width-7, 1, "easeInOutQuint" );
			this.lockButton.tween("_x", w-this.closeButton._width-7-14, 1, "easeInOutQuint" );
			this.dragButton.tween("_width", w, 1, "easeInOutQuint");
			this.stickyText.tween("_height", h-50, 1, "easeInOutQuint" );
			this.resizeHandles.slideTo ( w-5, h-5, 1, "easeInOutQuint")
			this.backing.tween("_width", w, 1, "easeInOutQuint");
			this.backing.tween("_height", h, 1, "easeInOutQuint");
		}else
		{
			this.w = w;
			this.h = h;
			this.stickyText._width = w-7;
			this.closeButton._x = w-this.closeButton._width-7;
			this.lockButton._x = this.closeButton._x - 14;
			this.dragButton._width = w;
			this.stickyText._height = h-50;
			this.resizeHandles._x = w-5;
			this.resizeHandles._y = h-5;
			this.backing._width = w;
			this.backing._height = h;
		}
		
	}
		
	public function getSize():Object
	{
		return {w:this.w, h:this.h};
	}
	
	public function setColor ( colIn:Number )
	{
		var col:Color = new Color ( backing );
			col.setRGB ( colIn );
	}
	
	public function getColor()
	{
		var col:Color = new Color(backing);
		return col.getRGB().toString(16);
	}
	
	
	
}