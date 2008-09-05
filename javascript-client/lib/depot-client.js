
var Depot = {
 parseXml: function(text) {
  return Try.these(
    function() { 
     return new DOMParser().parseFromString(text, 'text/xml'); 
    },
    function() { 
      var xmldom = new ActiveXObject('Microsoft.XMLDOM'); 
      xmldom.loadXML(text); 
     return xmldom; 
    });
 }
};

Depot.Object = Class.create();  
Depot.Object.prototype = {
 initialize: function(path) {this.path = path},
 onGet: defaultRetrieveHandler,
 onCreate: defaultCreateHandler,
 onDelete: defaultDeleteHandler,
 onNotFound: defaultNotFoundHandler,
 onServerError: defaultServerHandler,
 onBadRequest: defaultBadRequestHandler,  
 onException: defaultExceptionHandler,  

 configOptions: function(opts) {
    var options = {
      asynchronous: true,
      requestHeaders:  ["X-Snepo-Request-Kludge","GET"],
      method: 'get',
      onComplete: delegateToDepotHandlers
    };

    if (!options.onGet) options.onGet = this.onGet;
    if (!options.onCreate) options.onCreate = this.onCreate;
    if (!options.onDelete) options.onDelete = this.onDelete;
    if (!options.onBadRequest) options.onBadRequest = this.onBadRequest;
    if (!options.onNotFound) options.onNotFound = this.onNotFound;
    if (!options.onServerError) options.onServerError = this.onServerError;        

    if (!options.onException) options.onException = this.onException;  


    Object.extend(options, opts || {});    
    options.requestHeaders.push("X-Snepo-Client", "Ajax Depot Client/0.1");

    // please note that only one Snepo Directive can be sent at a time
    // subsequent values will overwrite the previous value.
    if(options.metaDataOnly) {
      options.requestHeaders.push("X-Snepo-Directive","META_ONLY");
    }
    if(options.sizeOnly) {
      options.requestHeaders.push("X-Snepo-Directive","DIR_SIZE"); 
    }
    if(options.createDirectory) {
      options.requestHeaders.push("X-Snepo-Directive","CREATE");
    }
    if(options.deleteDirectory) {
      options.requestHeaders.push("X-Snepo-Directive","DELETE_DIR");
    }
    if(options.destroyDirectory) {
      options.requestHeaders.push("X-Snepo-Directive","DESTROY_DIR");
    }
    return options;
 },
 
 get: function (idval, opts) {
    this.executeQuery(this.path + "?id=" + idval, opts);
  },
 getAll: function(opts) {
    this.executeQuery(this.path, opts); 
  },
 getFirst: function(opts) {
    this.executeQuery(this.path + "?id=(min)",opts);
  },
 getLast: function(opts) {
    this.executeQuery(this.path + "?id=(max)",opts);    
  },
 getRange: function(from, to, opts) {
    var f = from || "";
    var t = to   || "";
    var range = from + ".." + to;
    this.executeQuery(this.path + "?id=" + range, opts);
  },
 
 newDirectory: function(opts) {
    var o = opts || {};
    o.createDirectory = true;
    o.method = 'post';
    // NOTE: the X-Snepo-Request-Kludge header is present because
    // Some browsers only support POST and GET through XmlHTTPRequest
    // calls. The presence of the header affects the way that the depot
    // server handles incoming requests. All requests that contain the 
    // X-Snepo-Request-Kludge header MUST be sent as POST requests in
    // order for HTTP Digest authentication to work.
    o.requestHeaders = ["X-Snepo-Request-Kludge","PUT"];
    this.executeQuery(this.path,o);
  },
 
 create: function(data, opts) {
    var o      = opts || {};
    o.method   = 'post';
    o.requestHeaders = ["X-Snepo-Request-Kludge","PUT"];
    o.postBody = data;
    this.executeQuery(this.path,o);
  },

 update: function(id,data, opts) {
    var o      = opts || {};
    o.method   = 'post';
    o.requestHeaders = ["X-Snepo-Request-Kludge","PUT"];
    o.postBody = data;
    this.executeQuery(this.path + "?id=" + id,o);
  },

 destroy: function(id, opts) {
    var o      = opts || {};
    o.method   = 'post';
    o.requestHeaders = ["X-Snepo-Request-Kludge","DELETE"];
    this.executeQuery(this.path + "?id=" + id,o);
  },

 deleteDirectory: function(path, opts) {
    var o      = opts || {};
    o.method   = 'post';
    o.requestHeaders = ["X-Snepo-Request-Kludge","DELETE",
                        "X-Snepo-Directive", "DELETE_DIR"];
    this.executeQuery(this.path,o);
  },

 destroyDirectory: function(path, opts) {
    var o      = opts || {};
    o.method   = 'post';
    o.requestHeaders = ["X-Snepo-Request-Kludge","DELETE",
                        "X-Snepo-Directive", "DESTROY_DIR"];
    this.executeQuery(this.path,o);
  },
  
 size: function(opts) {
    var o      = opts || {};
    o.sizeOnly = true;
    this.executeQuery(this.path,o);
  },

 executeQuery: function(qstring,options) {    
    var http = new Ajax.Request(qstring,this.configOptions(options));
  }
};


/*-----------------------------------------------------------*/

function delegateToDepotHandlers(opts, httpreq, jsonHeader) {    
    var data   = httpreq.responseText;
    switch(httpreq.status) {
      case 200:
	var resp   = parseDepotResponse(data);
        opts.onGet(resp, httpreq, jsonHeader);
	break;
      case 201: 
        opts.onCreate(data,httpreq,jsonHeader);
	break;
      case 223: 
        opts.onDelete(data,httpreq,jsonHeader);
	break;
      case 401:
	opts.onBadRequest(data, httpreq, jsonHeader);
	break;
      case 404:
        opts.onNotFound(data, httpreq,  jsonHeader);
	break;
      case 500:
	opts.onServerError(data, httpreq,jsonHeader);
	break;
   }
}

function parseDepotResponse(data) {  
  var doc      = Depot.parseXml(data);
  var entries  = $A(doc.getElementsByTagName('entry'));
  var parsedEntries = [];
  entries.each(function(entry) {
      var metaxml  = $A(entry.firstChild.childNodes); // /entry/meta-data/
      var dataobj  = {isDirectory: false};
      assignMetaData(dataobj,metaxml);      
      var rawdat  = entry.getElementsByTagName('data')[0].firstChild.nodeValue;
      dataobj['data'] = rawdat;
      parsedEntries.push(dataobj);
    });
  
  var dirs     = $A(doc.getElementsByTagName('sub-directory'));
  if(dirs) {
  dirs.each(function(dir) {
      var dirobj  = {isDirectory: true};
      var metaxml = $A(dir.childNodes);
      assignMetaData(dirobj, metaxml);
      parsedEntries.push(dirobj);
    });
  }
  if(parsedEntries.length == 1) {
    return parsedEntries[0];
  } else {
    return parsedEntries;
  }
}

function assignMetaData(dataobj, metaxml) {
  metaxml.each(function(metaDatum) {
      dataobj[metaDatum.nodeName.camelize()] = metaDatum.firstChild.nodeValue;
    });
}

function defaultRetrieveHandler(depot, resp, hdr) { 
  var str = depot.objectName + " : " + depot.contentType + " \n" + depot.data
  alert(str);
}


function defaultNotFoundHandler(dat, res) {alert('not found \n' + dat);}
function defaultServerHandler(dat, res) {alert('server error \n' + dat);}
function defaultBadRequestHandler(dat, res) {alert('bad req \n' + dat);}
function defaultCreateHandler(dat, res) {return true;}
function defaultDeleteHandler(dat, res) {return true;}
function defaultExceptionHandler(req, exc) {alert('exception ' + exc.toString());}
