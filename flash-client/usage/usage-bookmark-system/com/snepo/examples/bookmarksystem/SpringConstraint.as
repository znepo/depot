class com.snepo.examples.bookmarksystem.SpringConstraint extends MovieClip
{
	
	public var pointA:MovieClip;
	public var pointB:MovieClip;
	public var restLength:Number;
	public var drawable:Boolean;
	public var tension:Number;
	public var color:Number;
	public var alpha:Number;
	public var width:Number;
	public var lineColor:Number;
	public var lineAlpha:Number;
	public var lineWidth:Number;
	public var lineActiveColor:Number;
	public var lineActiveAlpha:Number;
	public var lineActiveWidth:Number;
	
	public function SpringConstraint ( ptA:MovieClip, ptB:MovieClip, rl:Number, drawable:Boolean )
	{
		this.pointA = ptA;
		this.pointB = ptB;
		this.pointA.springs.push ( this );
		this.pointB.springs.push ( this );
		this.restLength = rl;
		this.drawable = drawable;
		//this.tension = 0.5;
		this.tension = 0.05;
		this.color = 0xFFFFFF;
		this.alpha = 20;
		this.width = 5;
	}
	
	public function constrain ( )
	{
		var x1:Number = this.pointB._x;
		var y1:Number = this.pointB._y;
		
		var x2:Number = this.pointA._x;
		var y2:Number = this.pointA._y;
		
		var dx:Number = x2 - x1;
		var dy:Number = y2 - y1;

		var d:Number = Math.sqrt( dx * dx + dy * dy );

		//var restLength = MathUtils.calculateChordLength ( a.lockedRadius, avoidances[a.level].length );
		//var restLength = 95;
		
		var ds:Number = this.tension * (d - restLength ) / d;
		
			dx = dx * ds;
			dy = dy * ds;

			x1 += dx;
			x2 -= dx;
			y1 += dy;
			y2 -= dy;
			
			
		
		if ( this.pointA.active && this.pointB.active )
		{
			if ( !this.pointA.dragging )
			{
				this.pointA.tx = x2;
				this.pointA.ty = y2;
			}
			
			if ( !this.pointB.dragging )
			{
				this.pointB.tx = x1;
				this.pointB.ty = y1;
			}
		}
	}
	
	public function update ( )
	{
		this.pointA.snapToRing();
		this.pointB.snapToRing();
	}
	
	public function draw( g:MovieClip )
	{
		g.lineStyle ( this.width, this.color, this.alpha );
		if ( this.drawable )
		{
			g.moveTo ( this.pointA._x, this.pointA._y );
			g.lineTo ( this.pointB._x, this.pointB._y );
		}
	}
	
	
}