class com.snepo.examples.bookmarksystem.Node extends MovieClip
{
	
	public var level:Number;
	public var lockedRadius:Number;
	public var label:String;
	public var backing:MovieClip;
	public var labelText:TextField;
	public var labelBacking:MovieClip;
	public var URL:String;
	public var isOpen:Boolean;
	public var springs:Array;
	
	public var id:Number;
	public var tx:Number = 0;
	public var ty:Number = 0;
	public var ax:Number = 10;
	public var ay:Number = 10;
	public var friction = 0.8;
	public var releaseTimer:Number = 0;
	public var releaseThreshold:Number = 200;
	public var doubleClickTimer:Number = 0;
	public var doubleClickThreshold:Number = 400;
	public var parent:MovieClip;
	public var children:Array;
	public var returnRadius:Number;
	public var active:Boolean;
	public var highlight:MovieClip;
	private var type:String;
	public var value:String;
	
	public var $onPress:Function;
	public var $onRelease:Function;
	public var Icon:MovieClip;
	
	public var addFolder:MovieClip;
	public var deleteFolder:MovieClip;
	public var addLink:MovieClip;
	public var deleteLink:MovieClip;	
	
	private var buttonProps:Array = [];
	private var bpMC:Array;
	
	public function Node ()
	{
		springs = [];
		children = [];
		
		bpMC = [addFolder, deleteFolder, addLink, deleteLink];
		
		buttonProps[0] = {};
		buttonProps[0].x = bpMC[0]._x;
		buttonProps[0].y = bpMC[0]._y;
		
		buttonProps[1] = {};
		buttonProps[1].x = bpMC[1]._x;
		buttonProps[1].y = bpMC[1]._y;
		
		buttonProps[2] = {};
		buttonProps[2].x = bpMC[2]._x;
		buttonProps[2].y = bpMC[2]._y;
		
		buttonProps[3] = {};
		buttonProps[3].x = bpMC[3]._x;
		buttonProps[3].y = bpMC[3]._y;
		
		for ( var i = 0; i < buttonProps.length; i++ )
		{
			bpMC[i]._x = 0;
			bpMC[i]._y = 0;
			//bpMC[i]._alpha = 0;
			bpMC[i].hit.onPress = function()
			{
				this._parent._parent.toggleFormMasks ( this._parent.form );
			}
		}
		
		this.active = true;
		labelText.autoSize = "center";
		highlight._alpha = 0;
		
		this.backing.onPress = this.$onPress = function()
		{
			this._parent.dragging = true;
			this._parent.releaseTimer = getTimer();
			if ( getTimer() - this._parent.doubleClickTimer < this._parent.doubleClickThreshold )
			{
				if ( this._parent.getType() == "link" )
				{
					//trace ( this._parent.value );
					getURL ( this._parent.value, "_blank" );
				}
			}
			this._parent.doubleClickTimer = getTimer();
				
		}
		
		this.backing.onRelease = this.backing.onReleaseOutside = this.$onRelease = function()
		{
			if ( getTimer() - this._parent.releaseTimer < this._parent.releaseThreshold )
			{
				
				this._parent._parent.sizeToLevel ( (this._parent.level) );
				if ( this._parent.level > 0 ) 
				{ 
					this._parent._parent.hideNodesByID ( this._parent.level, this._parent.id ); 
				}
				
				if ( !this._parent.isOpen && this._parent.level > 0 )
				{
					_level0.handleGet ( this._parent.URL, this._parent );
					this._parent.isOpen = true;
				}else
				{
					this._parent._parent.focusOn ( this._parent );
					
				}
				
				this._parent._parent.handleButtons ( this._parent ); 
			}
			this._parent.dragging = false;
		}
		
		this.onEnterFrame = function()
		{
			if ( this.dragging )
			{
				this.tx = this._parent._xmouse;
				this.ty = this._parent._ymouse;
				
				/*var dx:Number = this.parent._x - this._x;
				var dy:Number = this.parent._y - this._y;
				
				var d = Math.sqrt ( dx*dx + dy*dy );*/
			}
			
			if ( !this.active )
			{
				this._x += ( this.parent._x - this._x ) / 10;
				this._y += ( this.parent._y - this._y ) / 10;
			}
		}
		
	}
	
	public function toggleButtons ( aBool:Boolean )
	{
		for ( var i = 0; i < this.buttonProps.length; i++ )
		{
			this.buttonProps[i].mc.stopTween();
			if ( aBool == true )
			{
				this.bpMC[i].slideTo ( this.buttonProps[i].x, this.buttonProps[i].y, 1, "easeInOutQuint", 0.2+i/20 );
				//this.bpMC[i].alphaTo ( 100, 0.7, "easeInOutQuint", 0.2+i/10 );
			}else
			{
				bpMC[i].slideTo ( 0, 0, 1, "easeInOutQuint", 0.2+i/20 );
				//bpMC[i].alphaTo ( 0, 0.7, "easeInOutQuint", 0.2+i/20 );
				
			}
			
		}
		
		if ( !aBool ) toggleFormMasks ( -1 );
	}
	
	public function toggleFormMasks( form )
	{
		for ( var i = 0; i < this.bpMC.length; i++ )
		{
			if ( this.bpMC[i].form == form )
			{
				this.bpMC[i].form.mask.tween("_width", 250, 1, "easeInOutQuint" );
			}else
			{
				this.bpMC[i].form.mask.tween("_width", 1, 1, "easeInOutQuint" );
			}			
		}
	}
	
	public function setType ( type:String )
	{
		this.type = type;
		if ( this.type == "directory" )
		{
			this.Icon.gotoAndStop("plus");
			this.deleteLink._alpha = 30;
			this.deleteLink.enabled = false;
			
		}else
		{
			this.Icon.gotoAndStop("doc");
			this.deleteFolder._alpha = 30;
			this.deleteFolder.enabled = false;
			this.addFolder._alpha = 30;
			this.addFolder.enabled = false;
			this.deleteLink._alpha = 30;
			this.deleteLink.enabled = false;
		}
	}
	
	public function getType():String
	{
		return this.type;
	}
	
	public function attachToRing ( ring:Number )
	{
		this.level = ring;
		this.lockedRadius = (this._parent.zoomLevelSpread/2) * this.level;
		
		var radiusAdjust:Number = Math.random() * 60 - 30;
		
		//this.lockedRadius += radiusAdjust;
		this.returnRadius = this.lockedRadius;
		
		this.snapToRing();
	}
	
	public function snapToRing()
	{
		if ( this.active )
		{
			var dx = this.tx - 0;
			var dy = this.ty - 0;
			
			var angle:Number = Math.atan2 ( dy, dx ) / Math.PI * 180;
			var tx:Number = Math.cos ( angle * Math.PI / 180 ) * this.lockedRadius;
			var ty:Number = Math.sin ( angle * Math.PI / 180 ) * this.lockedRadius;
			
			this._x = tx;
			this._y = ty;
		}
	}
	
	public function setLabel ( aLabel:String ) :Void
	{
		this.label = aLabel;
		this.labelText.text = aLabel;
		this.labelBacking._width = this.labelText.textWidth + 8;
	}
	
	public function getLabel():String
	{
		return this.label;
	}
	
	public function hideCompletely ( a )
	{
		this._visible = false;
	}
	
	

}