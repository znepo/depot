/* 
//
//	FILE:			DepotChat.as
//
//	DESCRIPTION:	Depot Chat Client class
//
//	REVISION:		23 08 2006	v0.1 alpha
//				
//	TO DO:			Online user management
//					UI Enhancements
//	
//	COPYRIGHT:		COPYRIGHT (C) 2005-2006 Snepo Research Pty. Ltd., ALL RIGHTS RESERVED
*/

//// IMPORTS

import com.snepo.depot.*;

import mx.controls.TextArea;
import mx.controls.List;
import mx.controls.Button;
import mx.utils.Delegate;
import mx.events.EventDispatcher;

class depotchat.DepotChat
{
	
	/* == UI COMPONENTS == */
	
	private var UI_ChatText:TextArea;
	private var UI_MessageText:TextArea;
	private var UI_OnlineUsers:List;
	private var UI_ClearButton:Button;
	private var UI_SendButton:Button;
	
	/* == DEPOT PARAMETERS == */
	
	private var $chaturl:String;
	private var $userurl:String;
	private var $dataurl:String;
	private var DP:Depot;
	
	/* == APPLICATION PARAMETERS == */
	
	private var init:Boolean = false; // true for first run only, creates depot directories;
	private var $viewIndex:Number;
	private var $firstView:Boolean = true;
	private var chatpoll:Number;
	private var $NAME:String;
	
	/* == EVENTDISPATCHER MIXINS == */
	
	public function addEventListener(){};
	public function removeEventListener(){};
	public function dispatchEvent(){};
	
	public function DepotChat ( _c:TextArea, _m:TextArea, _o:List, _cb:Button, _sb:Button, _n:String )
	{
		EventDispatcher.initialize ( this );
		
		UI_ChatText = _c;
		UI_MessageText = _m;
		UI_OnlineUsers = _o;
		UI_ClearButton = _cb;
		UI_SendButton = _sb;
		
		$NAME = _n;
		
		$chaturl = "usage-chat/chatdata/";
		$userurl = "usage-chat/userdata/";
		$dataurl = "usage-chat/etcdata/";
		
		DP = new Depot ( "localhost", 2323 );
		DP.onGet = Delegate.create ( this, _onGet );
		if ( this.init )
		{
			DP.newDirectory ( $chaturl );
			DP.newDirectory ( $userurl );
			DP.newDirectory ( $dataurl );
		}
		
		setupUI();
		sendNotification ( $NAME + " has entered the chat." );
		refreshChat();
		chatpoll = setInterval ( this, "refreshChat", 1000 );
		
	}
	
	private function setupUI ()
	{
		UI_ClearButton.addEventListener("click", Delegate.create ( this, _clearClick ));
		UI_SendButton.addEventListener("click", Delegate.create ( this, _sendClick ));
	}
	
	private function _clearClick ( e:Object )
	{
		UI_MessageText.text = "";
	}
	
	private function _sendClick ( e:Object )
	{
		if ( UI_MessageText.text.length > 0 )
		{
			sendMessage ( $NAME, UI_MessageText.text );
		}
	}
	
	private function sendMessage ( from:String, msg:String )
	{
		DP.create ( $chaturl, {name:from, message:msg} );
		UI_MessageText.text = "";
		refreshChat();
	}
	
	private function sendNotification ( msg:String )
	{
		DP.create ( $chaturl, {name:"~NOTIFICATION", message:msg} );
		refreshChat();
	}
	
	private function _onGet ( e:Array )
	{
		if ( $firstView )
		{
			$viewIndex = e.length-1;
			if ( $viewIndex < 0 ) $viewIndex = 0;
			$firstView = false;
		}
		
		//UI_ChatText.text = "";
		for ( var i = $viewIndex; i < e.length; i++ )
		{
			if ( e[i].name != "~NOTIFICATION" )
			{
				UI_ChatText.text += "<b><i>" + e[i].name + " says : </i></b> " + e[i].message + "<br>";
			}else
			{
				UI_ChatText.text += "<font color='#FF0000'><b><i>" + e[i].message + "</i></b></font><br>";
			}
		}
		
		$viewIndex = e.length;
		
		UI_ChatText.vPosition = UI_ChatText.maxVPosition;
		
	}
	
	public function refreshChat ()
	{
		//trace ( "refreshing" );
		DP.get ( $chaturl );
	}
	
}