import mx.utils.Delegate;
class com.snepo.comments.Comment extends MovieClip
{
	
	private var commentTitle:TextField;
	private var commentBody:TextField;
	private var nameField:TextField;
	private var backing:MovieClip;
	private var mask:MovieClip;
	private var comment:Object;
	
	public var isExpanded:Boolean;
	public var expandButton:MovieClip
	
	
	public function Comment ()
	{
		comment = {};
		isExpanded = true;
		commentBody._visible = false;
		expandButton.onPress = function()
		{
			this._parent.toggleExpansion();
		}
	}
	
	public function toggleExpansion()
	{
		if ( arguments[0] )
		{
			this.isExpanded = arguments[0];
		}else
		{
			this.isExpanded = !this.isExpanded;
		}
		if ( !this.isExpanded )
		{
			var newHeight:Number = 50 + commentBody._height;
			commentBody._visible = true;
			backing.tween("_height", newHeight, 0.7, "easeInOutExpo", 0, Delegate.create ( this, updateScrollBar) );
			mask.tween("_height", newHeight, 0.7, "easeInOutExpo" );
			this._parent._parent._parent.container.contentY = -this._y;
		}else
		{
			var newHeight:Number = 36;
			backing.tween("_height", newHeight, 0.7, "easeInOutExpo", 0, Delegate.create ( this, updateScrollBar) );
			mask.tween("_height", newHeight, 0.7, "easeInOutExpo", Delegate.create ( this, hideBlockText) );
		}
		
	
	}
	
	public function updateScrollBar()
	{
		this._parent._parent._parent.scrollBar.update();
	}
	
	public function hideBlockText()
	{
		commentBody._visible = false;
	}
	
	public function setCommentProperties ( inComment:Object )
	{
		comment.name = inComment.name;
		comment.title = inComment.title;
		comment.body = inComment.body;
		if ( comment.title.length > 32 ) comment.title = comment.title.substring(0, 35) + "...";
		commentTitle.htmlText = "<b>" + comment.title + "</b>";
		nameField.htmlText = comment.name;
		commentBody.htmlText = comment.body;
		commentBody._height = commentBody.textHeight + 12;
		
	}
	
	public function getCommentProperties():Object
	{
		return comment;
	}
	
}