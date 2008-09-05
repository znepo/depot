import com.snepo.depot.*;

class com.snepo.depot.DepotConnector extends Depot
{
	[IconFile("DepotConnector.png")]
		
	[Inspectable(name="DepotHost", defaultValue="localhost")]
	public var _host:String;
	
	[Inspectable(name="DepotPort", defaultValue=2323)]
	public var _port:Number;
	
	[Inspectable(name="DepotURL")]
	public var URL:String;
	
	/*[Inspectable(name="onGetHandler", type=Function)]
	public var _onGet:Function;
	
	[Inspectable(name="onCreateHandler", type=Function)]
	public var _onCreate:Function;
	
	[Inspectable(name="onCreateDirectoryHandler", type=Function)]
	public var _onCreateDirectory:Function;
	
	[Inspectable(name="onErrorHandler", type=Function)]
	public var _onError:Function;
	
	[Inspectable(name="onConnectHandler", type=Function)]
	public var _onConnect:Function;*/
	
	public var Icon:MovieClip;
	public var _parent:MovieClip;
	
	public function DepotConnector()
	{
		super ( _host, _port );
		this.onConnect = _onConnect;
		this.onError = _onError;
		this.onGet = _onGet;
		this.onCreate = _onCreate;
		this.onCreateDirectory = _onCreateDirectory;
		Icon._visible = false;
		
	}
	
}