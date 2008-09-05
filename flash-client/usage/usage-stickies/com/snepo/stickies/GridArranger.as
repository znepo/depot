class com.snepo.stickies.Arranger
{
	public var xNum:Number;
	public var yNum:Number;
	public var xSpacing:Number;
	public var ySpacing:Number;
	public var xCtr:Number = 0;
	public var yCtr:Number = 0;
	
	public var gridItems:Array;
	
	public function Arranger(items:Array)
	{
		gridItems = items;
		xNum = Math.ceil ( Math.sqrt(items.length));
		yNum = Math.ceil ( Math.sqrt(items.length));
		xSpacing = 5;
		ySpacing = 5;
	}
	
	public function GridArrange()
	{

		var xWidth = Stage.width / (xNum+1)// + ((xNum+1)*xSpacing));
		var yWidth = ((Stage.height-25) / (yNum));
		
		for ( var i = 0; i < gridItems.length; i++ )
		{
			if ( !gridItems[i].locked )
			{
				gridItems[i].setSize ( xWidth, yWidth, true );
				gridItems[i].slideTo( xCtr*xWidth, yCtr*yWidth, 1, "easeInOutQuint" );
				gridItems[i].swapDepths (i);
				if ( xCtr++ >= xNum )
				{
					yCtr++;
					xCtr = 0;
				}
			}
		}
		
	}
	
	
}