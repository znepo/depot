class com.snepo.comments.AccordianComments extends MovieClip
{
	private var _comments:Array;
	private var context:MovieClip;
	
	public function AccordianComments(context:MovieClip)
	{
		_comments = [];
		this.context = context;
		this.context.$ac = this;
		this.context.onEnterFrame = function()
		{
			var $comments:Array = this.$ac.getComments();
			for ( var i = 0; i < $comments.length; i++ )
			{
				if ( i > 0 )
				{
					var $comment = $comments[i];
					$comment._y += ( ( $comments[i-1]._y + $comments[i-1].backing._height + 4) - $comment._y ) / 2;
				}
			}
		}
	}
	
	public function registerComment ( comment:MovieClip )
	{
		_comments.push ( comment );
	}
	
	public function getComments():Array
	{
		return _comments;
	}
	
	public function getCommentAt ( index:Number ):MovieClip
	{
		return _comments[index];
	}
	
	public function clearAll()
	{
		for ( var i = 0; i < _comments.length; i++ )
		{
			_comments[i].removeMovieClip();
		}
		
		_comments = [];
	}
	
	public function toggleAll ( bool:Boolean )
	{
		for ( var i = 0; i < _comments.length; i++ )
		{
			_comments[i].toggleExpansion(bool);
		}
	}
	
	
}