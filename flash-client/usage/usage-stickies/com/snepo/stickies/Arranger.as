/* 
 *
 *	FILE:			Arranger.as
 *
 *	DESCRIPTION:	Arranger class for Depot "Stickies" Usage Example
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

class com.snepo.stickies.Arranger
{
	public var xNum:Number;
	public var yNum:Number;
	public var xSpacing:Number;
	public var ySpacing:Number;
	public var xCtr:Number = 0;
	public var yCtr:Number = 0;
	
	public var gridItems:Array;
	
	// Register the "Stickies" array to the arranger.
	
	public function Arranger(items:Array)
	{
		gridItems = items;
		xNum = Math.ceil ( Math.sqrt(items.length));
		yNum = Math.ceil ( Math.sqrt(items.length));
		xSpacing = 5;
		ySpacing = 5;
	}
	
	// Arrange in a "Fan".
	
	public function fanArrange()
	{
		var cx:Number = Stage.width/2;
		var cy:Number = Stage.height/2-20;
		var yradius = cy-100;
		var xradius = cx-100;
		for ( var i = 0; i < gridItems.length; i++ )
		{
			if ( !gridItems[i].locked )
			{
				var angle:Number = ( i * ( 360 / gridItems.length ) );
				var xProj:Number = Math.sin ( angle * Math.PI / 180 ) * xradius;
				var yProj:Number = Math.cos ( angle * Math.PI / 180 ) * yradius;
				gridItems[i].setSize ( 140, 140, true );
				gridItems[i].slideTo ( cx+xProj-(140/2), cy+yProj-(140/2), 1, "easeInOutQuint", 0.2+i/10 );
				gridItems[i].swapDepths (i);			
			}
		}
	}
	
	// Arrange in a "Cascade".
	
	public function cascadeArrange()
	{
		var xDistance:Number = Stage.width;
		var yDistance:Number = Stage.height-25;
		var hypDistance:Number = Math.sqrt(xDistance*xDistance+yDistance*yDistance);
		var xStep:Number = 25;
		var yStep:Number = 25;
		
		var xSize:Number = (xDistance/2)-(gridItems.length*xStep);
		var ySize:Number = (yDistance/1.5)-(gridItems.length*yStep);
		
			xSize = xSize < 80 ? 80 : xSize;
			ySize = ySize < 80 ? 80 : ySize;
		
		for ( var i = 0; i < gridItems.length; i++ )
		{
			if ( !gridItems[i].locked )
			{
				gridItems[i].setSize ( xSize, ySize, true );
				gridItems[i].slideTo( i * xStep, i * yStep, 1, "easeInOutQuint", 0.2+i/10 );
				gridItems[i].swapDepths (i);
			}
		}
		
	}
	
	// Arrange in a "Grid".
	
	public function gridArrange()
	{

		var xWidth = Stage.width / (xNum+1)// + ((xNum+1)*xSpacing));
		var yWidth = ((Stage.height-25) / (yNum));
		
		for ( var i = 0; i < gridItems.length; i++ )
		{
			if ( !gridItems[i].locked )
			{
				gridItems[i].setSize ( xWidth, yWidth, true );
				gridItems[i].slideTo( xCtr*xWidth, yCtr*yWidth, 1, "easeInOutQuint", 0.2+i/10 );
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