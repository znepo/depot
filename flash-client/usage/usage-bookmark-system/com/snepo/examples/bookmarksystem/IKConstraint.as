class com.snepo.examples.bookmarksystem.IKConstraint extends MovieClip
{
	
	public var pointA:MovieClip;
	public var pointB:MovieClip;
	public var restLength:Number;
	public var drawable:Boolean;
	
	public function IKConstraint ( ptA:MovieClip, ptB:MovieClip, rl:Number, drawable:Boolean )
	{
		this.pointA = ptA;
		this.pointB = ptB;
		this.restLength = rl;
		this.drawable = drawable;
	}
	
	public function constrain ( )
	{
		var dx = this.pointA._x - this.pointB._x;
		var dy = this.pointA._y - this.pointB._y;
		
		var angle = Math.atan2( dy, dx );
		
		//trace ( this.pointB._x + Math.cos ( angle ) * this.restLength );
		if ( !this.pointA.dragging )
		{
			this.pointA.tx = this.pointB._x + Math.cos ( angle ) * this.restLength;
			this.pointA.ty = this.pointB._y + Math.sin ( angle ) * this.restLength;
		}
	}
	
	public function update ( )
	{
		this.pointA.snapToRing();
		this.pointB.snapToRing();
	}
	
	public function draw(  g:MovieClip )
	{
		if ( true )
		{
			g.moveTo ( this.pointA._x, this.pointA._y );
			g.lineTo ( this.pointB._x, this.pointB._y );
		}
	}
}