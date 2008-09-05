/*
class tweenManager for tweening prototypes
version 1.1.8
Ladislav Zigo,lacoz@web.de

pause unpause fixed
stop tween
growing of ints array
*/
class zigo.tweenManager{
	
	private var tweenList:Array;
	private var tweenHolder:MovieClip;
	private var playing:Boolean;
	private var now:Number;
	private var ints:Array;
	private var lockedTweens:Object;
	private var updateIntId:Number;
	private var updateTime:Number;
	private var pausedTime:Number;
	private var isPaused:Boolean;
	public var broadcastEvents:Boolean;
	public var autoStop:Boolean;
	public var autoOverwrite:Boolean;
	
	function tweenManager(){
		playing = false;
		isPaused = false;
		autoStop = false;
		broadcastEvents = false;
		autoOverwrite = true;
		ints = new Array();
		lockedTweens = new Object();
		tweenList = new Array();
		
	}	
	// 
	function set updateInterval(time:Number):Void{
		if (playing){
			deinit()
			updateTime = time
			init()
		}else{
			updateTime = time
		}
	}
	function get updateInterval():Number{
		return updateTime;
	}
	//  private methods 
	private function init():Void{
		if(updateTime > 0){
			updateIntId = setInterval(this,"update",updateTime);
		}else{
			// fix bug with "Simulate Download"
			// tweenHolder is still movieclip, but not on the stage		
			if(tweenHolder._name == undefined){
				// 
				tweenHolder = _root.createEmptyMovieClip("_th_",-6789); 
			}
			var tm = this;
			tweenHolder.onEnterFrame = function(){
				// this is faster because it eliminates usage of keyword "with" in update function
				tm.update.call(tm);
			}
		}
		playing = true;
		now = getTimer();
	}
	private function deinit():Void{
		playing = false;
		clearInterval(updateIntId);		
		delete tweenHolder.onEnterFrame;
	}
	
	private function update():Void{
		
		// loop all tween objects
		var t, i, j;
		i = tweenList.length;
		// broadcasting events
		if(broadcastEvents){
			// list of updated mcs
			var ut, et;
			ut = {};
			// list of ending mcs
			et = {};
		}
		
		while (i--){
			t = tweenList[i];
			if(t.ts + t.d > now){		
				// compute value using equation function
				if(t.ctm == undefined){
					// compute primitive value
					t.mc[t.pp] = t.ef(now-t.ts,t.ps,t.ch,t.d,t.e1,t.e2);
				}else{
					// compute color transform matrix 
					// stm is starting transform matrix, 
					// ctm is change in start & destination matrix 
					// ttm is computed (temporary) transform matrix
					// c is color object
					var ttm = {};
					for(j in t.ctm){
						ttm[j] = t.ef(now-t.ts,t.stm[j],t.ctm[j],t.d,t.e1,t.e2) 
					}
					t.c.setTransform(ttm);
				} 
				if(broadcastEvents){
					if(ut[targetpath(t.mc)] == undefined){
						ut[targetpath(t.mc)] = t.mc;
					}
				}
				// 
				if(t.cb.updfunc != undefined){
					t.cb.updfunc.apply(t.cb.updscope,t.cb.updargs);
				}
			} else {
				// end , set up the property to end value;
				if(t.ctm == undefined){
					t.mc[t.pp] = t.ps + t.ch;
				} else {
					var ttm = {};	
					for(j in t.ctm){
						ttm[j] = t.stm[j]+t.ctm[j];
					}
					t.c.setTransform(ttm);	
				}
				if(broadcastEvents){
					if(ut[targetpath(t.mc)] == undefined){
						ut[targetpath(t.mc)] = t.mc;
					}
					if(et[targetpath(t.mc)] == undefined){
						et[targetpath(t.mc)] = t.mc;
					}
				}
				
				// last update call
				
				if(t.cb.updfunc != undefined){
					t.cb.updfunc.apply(t.cb.updscope,t.cb.updargs);
				}
				// end tweens

				if (endt == undefined){
					var endt = new Array();
				}
				endt.push(i);
			}
		}
		// broadcast events (only 1 time for every mc)
		for (j in ut){
			ut[j].broadcastMessage('onTweenUpdate');
		}
		if(endt != undefined){
			endTweens(endt);
		}
		for (j in et){
			et[j].broadcastMessage('onTweenEnd');
		}
		// update timer 
		now = getTimer();
		// 	updatedisplay if in setinterval mode
		if (updateTime > 0){
			updateAfterEvent();
		}
		
	}
	private function endTweens(tid_arr:Array):Void{
		var cb_arr, tl, i, cb, j
		cb_arr = []
		// splice tweens from tweenlist 
		tl = tid_arr.length
		for (i = 0; i<tl; i++){
			cb = tweenList[tid_arr[i]].cb
			if(cb != undefined){
				var exec = true;
				//do not add callbacks that are in cb_arr
				for(var j in cb_arr){
					if (cb_arr[j] ==  cb){
						exec = false;
						break;
					}
				}
				//
				if(exec){
					cb_arr.push(cb);
				}
			}
			tweenList.splice(tid_arr[i],1);
		}
		// execute callbacks
		for (var i = 0; i<cb_arr.length;i++){
			cb_arr[i].func.apply(cb_arr[i].scope,cb_arr[i].args);
		}
		//
		if(tweenList.length==0){
			// last tween removed, erase onenterframe function
			deinit();
		}
	}
	//  public methods 
	public function addTween(mc:MovieClip,props:Array,pEnd:Array,
				sec:Number,eqFunc:Function,callback:Object,
				extra1:Object,extra2:Object):Void{
		var i, pp, addnew, j, t;
		if(!playing){
			init();
		}
		//
		for(i in props){
			pp = props[i];
			addnew = true;
			//
			if(pp.substr(0,4)!="_ct_"){
				// there is no color transform prefix, use primitive value tween
				//
				if(autoOverwrite){
					// find coliding tween and overwrite it 
					for (j in tweenList){
						t = tweenList[j];
						if(t.mc == mc && t.pp == pp){
							//
							t.ps = mc[pp];
							t.ch = pEnd[i] - mc[pp];
							t.ts = now;
							t.d = sec*1000;
							t.ef = eqFunc;
							t.cb = callback;
							t.e1 = extra1;
							t.e2 = extra2;
							addnew = false;						
							break;
						}
					}
				}
				if(addnew){	
				// not found add new
				tweenList.unshift({							
						  mc: mc,			// reference to movieclip		
						  pp: pp, 			// property
						  ps: mc[pp],			// starting value of property	
						  ch: pEnd[i] - mc[pp], 	// difference between starting and end value
						  ts: now, 			// start time of tween
						  d:  sec * 1000, 		// duration of tween
						  ef: eqFunc, 			// reference to easing equation function
						  cb: callback,			// callback object (function which is called at end of tween)
						  e1: extra1,			// extra 1 value (for elastic and bouce equation)
						  e2: extra2});			// extra 2 value (for elastic equation)
				}
			}else{
				// color trasform prefix found	
				// compute change matrix
				var c = new Color(mc);
				var stm = c.getTransform();
				// compute difference between starting and desionation matrix
				var ctm = {}
				for(j in pEnd[i]){
					// if is in destination matrix 
					if(pEnd[i][j] != stm[j] && pEnd[i][j] != undefined ){
						ctm[j] = pEnd[i][j] - stm[j];
					}
				}
				if(autoOverwrite){
				// find coliding tween and overwrite it 
				for (j in tweenList){
					t = tweenList[j];
					if(t.mc == mc && t.ctm != undefined){
							//
							t.c = c
							t.stm = stm	
							t.ctm =  ctm,
							t.ts = now;
							t.d = sec*1000;
							t.ef = eqFunc;
							t.cb = callback;
							t.e1 = extra1;
							t.e2 = extra2;
							addnew = false;						
							break;
						}
					}
				}
				if(addnew){	
				tweenList.unshift({
						mc:  mc,			//reference to movieclip
						c:   c,				//reference to movieclip color
						stm: stm,			//starting transform matrix
						ctm: ctm,			
						ts:  now,
						d:   sec * 1000,
						ef:  eqFunc,
						cb:  callback,
						e1:  extra1,
						e2:  extra2
					})
				}			
				
			}
		}
		if(broadcastEvents){
			mc.broadcastMessage('onTweenStart',props[i]); 
		}
		// execute start function (may be usefull with delayed tweens)
		if(callback.startfunc != undefined){
			callback.startfunc.apply(callback.startscope,callback.startargs)
		}
	
	}
	public function addTweenWithDelay(delay:Number,mc:MovieClip,props:Array,
									  pEnd:Array, sec:Number,eqFunc:Function,callback:Object,
									  extra1:Object,extra2:Object){
		//
		var il, intid;
		il = ints.length;
		intid = setInterval(function(obj){
			var isintsempty, i;
			
			obj.addTween(mc, props, pEnd, sec, eqFunc, callback, extra1, extra2);
			clearInterval(obj.ints[il].intid);
			obj.ints[il] = undefined;
			
			isintsempty = true
			for (i in obj.ints){
				if (obj.ints[i] != undefined){
					isintsempty = false;
					break;
				}
			}
			if (isintsempty){
				obj.ints = [];
			}
		},delay*1000,this);
		// array of waiting tweens, mc reference to movieclip, ..., intid setInterval function identifier
		ints[il] = {mc: mc, props: props, pend:pEnd, intid:intid, st: now, delay:delay*1000, args: arguments.slice(1)}
	}
	public function removeTween(mc:MovieClip,props:Array):Void{
		var all, i, j;
		all = false;
		if(props == undefined){
			// props are undefined, remove all tweens
			all = true;
		}
		i = tweenList.length; 
		while (i--){
			if(tweenList[i].mc == mc){
				if(all){
					tweenList.splice(i,1);
				}else{
					// removing tweening of properties
					for(j in props){
						if(tweenList[i].pp == props[j]){
							tweenList.splice(i,1);
							// props.splice(j,1) 
							// (because allows add same properties for same mc,
							// all tweens must be checked) 
						} else if (props[j] == "_ct_" && tweenList[i].ctm != undefined && tweenList[i].mc == mc){
							// removing of colorTransform tweens
							tweenList.splice(i,1);
						}
					}
				}
			}
		}
		i = ints.length;
		// delayed tweens 
		while(i--){
			if(ints[i].mc == mc)
				if(all){
					// REMOVE ALL
					clearInterval(ints[i].intid)
					ints[i] = undefined
				} else {
					// REMOVE PROPERTIES
					for(j in props){
						for(var k in ints[i].props){
							if(ints[i].props[k] == props[j]) {
								// remove tween properties + property end values
								ints[i].props.splice(k,1);
								ints[i].pend.splice(k,1);
							} 
						}
						if(ints[i].props.length == 0){
							clearInterval(ints[i].intid)
							// no properties to tween
						}
					}
				}
			}
		if(tweenList.length==0){
			// last tween removed, erase onenterframe function
			deinit();
		}
	}
	public function isTweening(mc:MovieClip):Boolean{
		for (var i in tweenList){
			if(tweenList[i].mc == mc){
				// mc found, so break loop
				return true;
				
			}
		}
		return false;
	}
	public function getTweens(mc:MovieClip):Number{
		var count = 0;
		for (var i in tweenList){
			if(tweenList[i].mc == mc){
				// found, increase count
				count++;
			}
		}
		return count;
	}
	public function lockTween(mc:MovieClip,bool:Boolean):Void{
		lockedTweens[targetpath(mc)] = bool;			
	}
	public function isTweenLocked(mc:MovieClip):Boolean{
		if(lockedTweens[targetpath(mc)] == undefined){
			return false;
		}else{
			return lockedTweens[targetpath(mc)];
		}			
	}
	public function pauseAll():Void {
	
			if (isPaused){
				return
			}
			isPaused = true
			pausedTime = now;
			// pause delayed
			for (var i in ints){
				clearInterval(ints[i].intid)
			}
			deinit()
	}
	public function unpauseAll():Void {
			if (!isPaused){
				return
			}
			var i, t;
			isPaused = false
			init();
			for (i in tweenList) {
				t = tweenList[i];
				// update start times 
				t.ts = now-(pausedTime-t.ts);
			}
			
			for (i in ints){
				if (ints[i] == undefined){
					continue
				}
				//
				var delay = ints[i].delay - (pausedTime - ints[i].st);
				var intid = setInterval(function(obj,id){
					obj.addTween.apply(obj, obj.ints[id].args);
					clearInterval(obj.ints[id].intid);
					obj.ints[id] = undefined;
				},delay,this,i);
				//
				ints[i].intid = intid;
				ints[i].st = now;
				ints[i].delay = delay;
			//
			}
			
			
	}
	public function stopAll():Void{
		for (var i in ints){
			clearInterval(ints[i].intid)
		}
		// stop all running tweens
		tweenList = new Array();	
		deinit();
	}
	public function toString():String{
		return "[AS2 tweenManager 1.1.8]";
	}
}