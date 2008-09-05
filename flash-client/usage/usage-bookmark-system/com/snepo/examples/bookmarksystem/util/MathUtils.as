class com.snepo.examples.bookmarksystem.util.MathUtils
{
	public static function calculateChordLength ( radius, numpoints )
	{
		var angle = ( 360 / numpoints );
		var p0 = 1 * angle;
		var p1 = 2 * angle;
		
		var p0x = Math.sin ( p0 * Math.PI / 180 ) * radius;
		var p0y = Math.cos ( p0 * Math.PI / 180 ) * radius;
		
		var p1x = Math.sin ( p1 * Math.PI / 180 ) * radius;
		var p1y = Math.cos ( p1 * Math.PI / 180 ) * radius;
		
		var dx = p0x - p1x;
		var dy = p0y - p1y;
		
		var d = Math.sqrt ( dx * dx + dy * dy );
		
		return d ;
	}
}