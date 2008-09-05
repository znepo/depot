import com.snepo.examples.bookmarksystem.Node;
import com.snepo.examples.bookmarksystem.SpringConstraint;
import com.snepo.examples.bookmarksystem.IKConstraint;
import com.snepo.examples.bookmarksystem.util.MathUtils;
import mx.utils.Delegate;

class com.snepo.examples.bookmarksystem.BookmarkSystem extends MovieClip
{
	
	public var nodes:Array;
	public var zoomLevel:Number;
	public var zoomLevelSpread:Number = 250;
	public var maxLevels:Number = 50;
	public var maxRings:Number = 10;
	public var rootNode:MovieClip;
	public var $this:MovieClip;
	public var restLength:Number;
	public var springs:Array;
	public var basenodes = [];
	public var rings:Array = [];
	public var graphics:MovieClip;
	public var maxSize:Number = 6000;
	public var minSize:Number = 2500;
	public var scaleRatio:Number;
	public var hitArea:MovieClip;
	public var hand:MovieClip;
	public var isPaused:Boolean;
	public var nodeMap:Array;
	
	public var lineWidth:Number = 5;
	public var lineColor:Number = 0xFFFFFF;
	public var lineActiveColor = 0xCC0000;
	public var lineActiveAlpha = 100;
	public var lineAlpha:Number = 20;
	public var nodeCtr = 0;
	private var hideCompletely:Function;
		
	public function BookmarkSystem ()
	{
		$this = this;
		zoomLevel = 10;
		nodes = [];
		basenodes = [];
		springs = [];
		nodeMap = [];
		
		graphics = this.createEmptyMovieClip("graphics", 0 );
		scaleRatio = maxSize / zoomLevelSpread;
		
		isPaused = false;
		
		hand = this._parent.attachMovie("Hand", "hand", 3 );
		createHitArea();
		
		
		
		for ( var i = 0; i < maxRings; i++ )
		{
			var ring:MovieClip = this.attachMovie("NodeMarker", "Ring" + i, i+1 );
				ring._x = 0;
				ring._y = 0;
				ring._width = ring._height = (i+1) * zoomLevelSpread;
				//ring._alpha = 35;
				rings.push ( ring );
		}
		
		//trace ( this._width + " : " + this._height );
		scaleRatio = maxSize / this._width * 5;
		this._width = this._height = maxSize;
		
		rootNode = this.attachMovie ( "Node", "RootNode", this.getNextHighestDepth() );
		rootNode.setLabel ( "Bookmarks" );
		rootNode.level = 0;
		rootNode.highlight.alphaTo ( lineActiveAlpha, 1 );
		rootNode.highlight.colorTo ( lineActiveColor, 1 );
		
		nodeMap[0] = [rootNode];
		
		/*addNode ( 1, {label:"Node 0_0"} );
		addNode ( 1, {label:"Node 0_1"} );
		addNode ( 1, {label:"Node 0_2"} );
		addNode ( 1, {label:"Node 0_3"} );
		
		addNode ( 2, {label:"Node 1_0"} );
		addNode ( 2, {label:"Node 1_1"} );
		addNode ( 2, {label:"Node 1_2"} );
		addNode ( 2, {label:"Node 1_3"} );
		
		addNode ( 3, {label:"Node 2_0"} );
		addNode ( 3, {label:"Node 2_1"} );
		addNode ( 3, {label:"Node 2_2"} );
		
		var rootRestLength:Number = MathUtils.calculateChordLength(zoomLevelSpread/2, 4 );
		
		addSpring ( 0, 1, rootRestLength, false ); addSpring ( 1, 0, rootRestLength, false );
		addSpring ( 1, 2, rootRestLength, false ); addSpring ( 2, 1, rootRestLength, false );
		addSpring ( 2, 3, rootRestLength, false ); addSpring ( 3, 2, rootRestLength, false );
		addSpring ( 3, 0, rootRestLength, false ); addSpring ( 0, 3, rootRestLength, false );
				
		addSpring ( 4, 5, zoomLevelSpread/2, false ); addSpring ( 5, 4, zoomLevelSpread/2, false );
		addSpring ( 5, 6, zoomLevelSpread/2, false ); addSpring ( 6, 5, zoomLevelSpread/2, false );
		addSpring ( 6, 7, zoomLevelSpread/2, false ); addSpring ( 7, 6, zoomLevelSpread/2, false );
		
		addSpring ( 1, 4, zoomLevelSpread/2, true ); addSpring ( 4, 1, zoomLevelSpread/2, false );
		addSpring ( 1, 5, zoomLevelSpread/2, true ); addSpring ( 5, 1, zoomLevelSpread/2, false );
		addSpring ( 1, 6, zoomLevelSpread/2, true ); addSpring ( 6, 1, zoomLevelSpread/2, false );
		addSpring ( 1, 7, zoomLevelSpread/2, true ); addSpring ( 7, 1, zoomLevelSpread/2, false );
		
		addSpring ( 8, 9, zoomLevelSpread/2, false ); addSpring ( 9, 8, zoomLevelSpread/2, false );
		addSpring ( 9, 10, zoomLevelSpread/2, false ); addSpring ( 10, 9, zoomLevelSpread/2, false );
		
		addSpring ( 8, 6, zoomLevelSpread/2, true ); addSpring ( 6, 8, zoomLevelSpread/2, false );
		addSpring ( 9, 6, zoomLevelSpread/2, true ); addSpring ( 6, 9, zoomLevelSpread/2, false );
		addSpring ( 10, 6, zoomLevelSpread/2, true ); addSpring ( 6, 10, zoomLevelSpread/2, false );
		
		*/
		
		this.onEnterFrame = function()
		{
			if ( !this.isPaused )
			{
				this.draw();
			}
		}
	}
	
	public function pause ()
	{
		this.isPaused = true;
	}
	
	public function resume()
	{
		this.isPaused = false;
	}
	
	public function addNode ( level:Number, props:Object )
	{
		if ( !nodeMap[level] || nodeMap[level].length == 0 ) nodeMap[level] = [];
		
		var d = this.getNextHighestDepth();
		var node:MovieClip = this.attachMovie ( "Node", "Node" + nodeCtr, d );
			node.attachToRing ( level );
			node.setLabel ( props.label );
			node.URL = props.url;
			node._x = props.x;
			node._y = props.y;
			node.parent = props.parent;
			node.value = props.value;
			props.parent.children.push ( node );
			node.id = nodeCtr;
			nodes.push ( node );
			nodeMap[level].push ( node );
			nodeCtr++;
			return node;
	}
	
	public function addSpring ( ptA:MovieClip, ptB:MovieClip, restLength:Number, drawable:Boolean )
	{
		//var spring:IKConstraint = new IKConstraint( ptA, ptB, restLength, drawable );
		var spring:SpringConstraint = new SpringConstraint( ptA, ptB, restLength, drawable );
			spring.lineActiveColor = this.lineActiveColor;
			spring.lineActiveAlpha = this.lineActiveAlpha;
			spring.lineWidth = 5;
			spring.lineColor = this.lineColor;
			spring.lineAlpha = this.lineAlpha;
			spring.lineActiveWidth = 12;
			springs.push ( spring );
	}
	
	public function addConstraint ( ptA:MovieClip, ptB:MovieClip, restLength:Number, drawable:Boolean )
	{
		var spring:IKConstraint = new IKConstraint( ptA, ptB, restLength, drawable );
		//var spring:SpringConstraint = new SpringConstraint( ptA, ptB, restLength, drawable );
			springs.push ( spring );
	}
	
	public function getNode ( index:Number )
	{
		return nodes[index];
	}
	
	public function getNodes():Array
	{
		return nodes;
	}
	
	public function sizeToLevel ( level:Number )
	{
		this.zoomLevel = level;
		//var newWidth = 5000 - ((zoomLevelSpread*2) * (level+3));
		//var newHeight = 5000 - ((zoomLevelSpread*2) * (level+3));
		//trace ( level + " : " + newWidth );
	
		//var newScale = 800 - ( 150 * (level+2) );
		//if ( newScale < 75 ) newScale = 75;
		
		//$this.tween(["_width", "_height"], [newWidth, newHeight], 3, "easeOutElastic" );
		//$this.tween(["_width", "_height"], [newWidth, newHeight], 1, "easeInOutExpo" );
		
		var width = maxSize - ((level * zoomLevelSpread * scaleRatio )) ;
		if ( width < minSize ) width = minSize;
		$this.tween(["_width", "_height"], [width, width], 1, "easeInOutExpo" );
		//$this.scaleTo ( newScale, 1, "easeInOutExpo" );
	}
	
	public function draw ()
	{
		this.graphics.clear();
		this.graphics.lineStyle ( this.lineWidth, this.lineColor, this.lineAlpha );
		for ( var i = 0; i < this.springs.length; i++ ) { springs[i].constrain(); springs[i].update(); }
		for ( var i = 0; i < this.springs.length; i++ ) springs[i].draw(this.graphics);
	}
	
	public function focusOn ( node )
	{
		var offsetX = node._x - Stage.width/2;
		var offsetY = node._y - Stage.height/2;
		
		$this.slideTo ( -offsetX, -offsetY, 1, "easeInOutExpo" );
		node.swapDepths ( node._parent.getNextHighestDepth() );
	}
	
	private function createHitArea()
	{
		hitArea = this._parent.createEmptyMovieClip("HIT_AREA", this._parent.getNextHighestDepth());
		hitArea._x = Stage.width/2;
		hitArea._y = Stage.height/2+10;
		hitArea._alpha = 0;
		hitArea.lineStyle(1, 0xffffff, 100 );
		hitArea.beginFill(0xffCC99, 70);
		hitArea.moveTo ( -Stage.width/2, -Stage.height/2 );
		hitArea.lineTo ( Stage.width/2, -Stage.height/2 );
		hitArea.lineTo ( Stage.width/2, Stage.height/2 );
		hitArea.lineTo ( -Stage.width/2, Stage.height/2 );
		hitArea.lineTo ( -Stage.width/2, -Stage.height/2 );
		hitArea.endFill();
		hitArea.bms = this;
		this.swapDepths ( hitArea );
		hitArea.useHandCursor = false;
		hitArea.hand = hand;
		hand._visible = false;
		hand.swapDepths(10);
		hitArea.maxDrag = 400;
		hitArea.onPress = function()
		{
			Mouse.hide();
			this.hand._visible = true;
			this.downPoint = {x:this._parent._xmouse, y:this._parent._ymouse};
			this.dragging = true;
			this.hand._x = this._parent._xmouse;
			this.hand._y = this._parent._ymouse;
			
			this.onEnterFrame = function()
			{
				this.hand._x = this._parent._xmouse;
				this.hand._y = this._parent._ymouse;
				this.bms._x -= (this._parent._xmouse - this.downPoint.x) / 30;
				this.bms._y -= (this._parent._ymouse - this.downPoint.y) / 30;
				
				var dx = this._parent._xmouse - this.bms._x;
				var dy = this._parent._ymouse - this.bms._y;
				
				var ang = Math.atan2 ( dy, dx ) / Math.PI * 180 - 90;
				this.hand.Arrow._rotation = ang
				
				/*var distanceDragged = Math.sqrt ( (this._parent._xmouse - this.downPoint.x) * (this._parent._xmouse - this.downPoint.x) + (this._parent._ymouse - this.downPoint.y) * (this._parent._ymouse - this.downPoint.y) )
				if ( distanceDragged > this.maxDrag )
				{
					this.onRelease();
				}*/
				
			}
			
			/*this.onEnterFrame = function()
			{
				this.hand._x = Math.round ( this._parent._xmouse );
				this.hand._y = Math.round ( this._parent._ymouse );
				this.bms._x -= ( this._parent._xmouse - this._x ) / 50;
				this.bms._y -= ( this._parent._ymouse - this._y ) / 50;
				var dx = this._parent._xmouse - this.bms._x;
				var dy = this._parent._ymouse - this.bms._y;
				
				var ang = Math.atan2 ( dy, dx ) / Math.PI * 180 - 90;
				this.hand.Arrow._rotation = ang;
			}*/
		}
		
		hitArea.onRelease = hitArea.onReleaseOutside = function()
		{
			this.hand._visible = false;
			Mouse.show();
			delete this.onEnterFrame;
		}
	}
	
	public function getNodesByLevel ( level:Number )
	{
		return nodeMap[level];
	}
	
	public function cleanUp ( parent, level )
	{
		if ( parent == rootNode ) return
		for ( var i = 0; i < nodeMap[level].length; i++ )
		{
			nodeMap[level][i].removeMovieClip();
			nodeMap[level].splice ( i, 1 );
		}
		
		parent.isOpen = false;
	}
	
	public function hideNodesByID ( level:Number, idToIgnore:Number )
	{
		this.pause();
		var nodes = this.getNodesByLevel ( level );
			for ( var i = 0; i < nodes.length; i++ )
			{
				if ( nodes[i].id != idToIgnore && nodes[i] != rootNode)
				{
					nodes[i].highlight.alphaTo ( 0, 1, "easeOutExpo" );
					for ( var j = 0; j < nodes[i].children.length; j++ )
					{
						//nodes[i].children[j]._visible = false;
						nodes[i].children[j].alphaTo ( 0, 1, "easeOutExpo", 0, Delegate.create ( nodes[i].children[j], nodes[i].children[j].hideCompletely ));
						nodes[i].children[j].lockedRadius = nodes[i].lockedRadius;
						nodes[i].children[j].active = false;
						
						delete nodes[i].children[j].backing.onPress;
						delete nodes[i].children[j].backing.onRelease;
						delete nodes[i].children[j].backing.onReleaseOutside;
						
						hideNodesByID ( nodes[i].children[j].level, -1 );
					}
					
					if ( nodes[i].getType() == "directory" )
					{
						nodes[i].Icon.gotoAndStop("plus");
					}else
					{
						
					}
					
					var spring = this.getSpringByNodes ( nodes[i], nodes[i].parent );
						spring.color = spring.lineColor;
						spring.alpha = spring.lineAlpha;
						spring.width = spring.lineWidth;
					
						
					
					
				}else
				{
					
					for ( var j = 0; j < nodes[i].children.length; j++ )
					{
						nodes[i].children[j].alphaTo ( 100, 1, "easeOutExpo" );
						nodes[i].children[j]._visible = true;
						nodes[i].children[j].lockedRadius = nodes[i].children[j].returnRadius;
						nodes[i].children[j].active = true;
						nodes[i].children[j].backing.onPress = nodes[i].children[j].$onPress;
						nodes[i].children[j].backing.onRelease = nodes[i].children[j].$onRelease;
						nodes[i].children[j].backing.onReleaseOutside = nodes[i].children[j].$onRelease;
						
						
					}
					
					if ( nodes[i].getType() == "directory" )
					{
						nodes[i].Icon.gotoAndStop("minus");
						//trace ( "oi " + nodes[i].Icon  );
					}else
					{
						
					}
					
					var spring = this.getSpringByNodes ( nodes[i], nodes[i].parent );
						spring.color = spring.lineActiveColor;
						spring.alpha = spring.lineActiveAlpha;
						spring.width = spring.lineActiveWidth;
					nodes[i].highlight.alphaTo ( spring.alpha, 1, "easeOutExpo" );
					nodes[i].highlight.colorTo ( spring.color, 1, "easeOutExpo" );
				
					
				}
				nodes[i].swapDepths ( nodes[i]._parent.getNextHighestDepth() );
			}
			
			rootNode.highlight.alphaTo ( lineActiveAlpha, 1 );
			rootNode.highlight.colorTo ( lineActiveColor, 1 );
			this.resume();
	}
	
	public function handleButtons ( node )
	{
		for ( var i = 0; i < this.nodes.length; i++ )
		{
			if ( this.nodes[i] == node )
			{
				//trace ( true + " : " + this.nodes[i].getLabel());
				this.nodes[i].toggleButtons(true);
				this.nodes[i].swapDepths ( this.nodes[i]._parent.getNextHighestDepth() );
			}else
			{
				//trace ( false + " : " + this.nodes[i].getLabel() );
				this.nodes[i].toggleButtons(false);
			}
		}
	}
	
	public function getSpringByNodes ( ptA:MovieClip, ptB:MovieClip )
	{
		for ( var i = 0; i < this.springs.length; i++ )
		{
			if ( this.springs[i].pointA == ptA && this.springs[i].pointB == ptB && this.springs[i].drawable )
			{
				return this.springs[i];
			}
			
			if ( this.springs[i].pointA == ptB && this.springs[i].pointB == ptA && this.springs[i].drawable )
			{
				return this.springs[i];
			}
		}
		return false;
	}
	
	public function refresh ( targetNode )
	{
	//	trace ( "refreshing " + targetNode );
		for ( var i = 0; i < targetNode.children.length; i++ )
		{
			targetNode.children[i].removeMovieClip();			
		}
		this.cleanUp();
		_level0.handleGet ( targetNode.URL, targetNode );
	}
	
}