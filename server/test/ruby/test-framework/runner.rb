require 'net/http'
require 'thread'
require 'yaml'
require 'rexml/document'

class String
  def /(num)
    exp = ".?" * num
    ary = self.scan(Regexp.new(exp,Regexp::MULTILINE))
    ary.pop
    ary
  end
end

# extending generic request to handle forced latency
module Net
  class HTTPGenericRequest
    attr_accessor :latency
    def write_header(sock,ver,path)
      buf = "#{@method} #{path} HTTP/#{ver}\r\n"
      canonical_each do |k,v|
        buf << k + ': ' + v + "\r\n"
      end
      buf << "\r\n"
      if latency        
        interval, amount = latency[0], latency[1].to_f
        minibufs = buf / interval
        minibufs.each {|minibuf|
          sleep amount
          sock.write minibuf
        }
      else
        sock.write buf      
      end
    end
  end
end

module DepotTest
  
  class Payload     
    class << self
      def alphabet
        c  = "a1"
        ab = ""
        100.times {
          ab << c
          c.succ!
        }
        ab
      end
      def gen(size=10)
        ab = alphabet
        st = ""
        size.times {
          st << ab[rand(ab.size)]
        }
        st      
      end
    end    
  end
  
  class Request
    attr_accessor :num_requests, :latency, :body, :strategy, :path, :assertions, :host, :headers
    
    def initialize
      @headers    ||= {}
      @headers["x-snepo-client"] = "Ruby Test Client/0.1"      
      @sent         = []
    end
    
    def []=(k,v)
      @headers[k] = v
    end
    
    def [](k)
      headers[k]
    end
    
    def body_data
      dat ||= case body 
                when :random then Payload::gen
                else body.to_s
              end
    end
    
    def make_request(http,dat)      
      if strategy == 'roundtrip'
        put_req         = http_request 'put'
        put_req.body    = dat
        put_req.latency = latency
        put_response    = http.request put_req
        yield run_assertion(assertions['put'], put_response, dat)
        
        get_req         = http_request 'get', "?id=#{put_response['x-object-id']}"
        get_req.latency = latency
        get_response    = http.request get_req
        yield run_assertion(assertions['get'], get_response, dat)
      else
        req         = http_request(strategy)
        req.latency = latency
        req.body    = dat if strategy == 'put'
        response    = http.request req
        yield run_assertion(assertions[strategy], response, dat)
      end      
    end
       
    def run_assertion(assertion,response,dat)
      report    = ""
      do_assert = lambda {|bool,name,msg| 
        if bool
          ""
        else
          "Assertion #{name} failed #{msg}\nRESPONSE WAS:\n #{response.body}\n"
        end
      }
      if assertion["code"]
        bool = assertion["code"] == response.code
        name = "resonse code"
        msg  = "expected #{assertion["code"]} got #{response.code}"
        report << do_assert.call(bool,name,msg)
      end
      if assertion["body"]
        body_dat = ""
        if assertion["xml"]
          begin
          body_doc = REXML::Document.new(response.body) 
          body_dat = body_doc.elements["/depot-result/entry/data"][0]
          rescue
            report << "ERROR! #{$!}\n"
          end
        else
          body_dat = response.body
        end
        report << do_assert.call(body_dat == dat, "body equal", "expected\n#{dat}\ngot #{body_dat}")
      end
      if assertion["headers"]
        assertion["headers"].each {|k,v|
          bool = response[k] == v
          name = "Header #{k}"
          msg  = "expected #{v} got #{response[k]}"
          report << do_assert.call(bool,name,msg)
        }
      end
      report
    end
    

    
    def to_s
      str = header_str
      if body
        str << "\r\n<#{body}: #{num_requests}>"
      end
      str
    end
    
    def header_str
      str =  "#{method.to_s.upcase} #{path} HTTP/1.1\r\n"
      headers.each {|k,v|
        str << "#{k}: #{v}\r\n"
      }
      str
    end
    
    def http_request(method,query="")
      req_c = case method 
               when 'put'    then Net::HTTP::Put
               when 'get'    then Net::HTTP::Get
               when 'delete' then Net::HTTP::Delete
               when 'post'   then Net::HTTP::Post
             end
      req_c.new(path + query, headers)
    end
  end
     
  class Runner
    attr_accessor :directory, :host, :port, :num_threads, :latency
    def initialize(directory, host, port)
      @directory   = directory
      @num_threads = 1   
      @host        = host
      @port        = port
#      @http        = 
    end
    def http
      Net::HTTP.new host, port
    end
    def run
      requests = []
      loads    = []
      Dir["#{directory}/load-*.yml"].each {|req_file|
        req_file.scan(/.*?load-(.*?)\.yml/) {|p|
          path = "/com/snepo/tests/#{p[0].gsub("-","/")}"
          File::open(req_file) {|f|
            loads << [YAML::load(f), path]
          }
        }
      }
      
      Dir["#{directory}/test-*.yml"].each {|req_file|
        File::open(req_file) {|f|
          r = YAML::load(f)
          if r.respond_to? :each
            r.each {|request|
              requests << request
            }
          else
            requests << r
          end
        }
      }

      benchmark("#{directory} data load") {
        loads.each { |l|
          send_data l
        }
      }
      
      benchmark("#{directory} tests") {
        requests.each {|r|
          multithreaded_request(r)      
        }
      }

    end
    
    def send_data(load_ary)
      load_data,path = load_ary
      load_data.each {|dat|
        begin
          if dat['method'] == 'DELETE'
            req = Net::HTTP::Delete.new(path,dat['headers'])
            puts "#{req.method} #{req.path}"
            http.request(req)                
          else
            req = Net::HTTP::Put.new(path,dat['headers'])
            http.request(req,dat['data'])      
          end
        rescue Object => exception
          puts exception
        end
      }
    end
    
    def send_request(request)
      if request.num_requests
        request.num_requests.times {|i|
          yield request, i
        }
      else
          yield request, nil
      end
    end
    
    private

    def benchmark(operation,&block)
      start = Time::now.to_f
      puts "Starting #{operation}..."
      block.call
      elapsed = Time::now.to_f - start
      puts "Finished #{operation} in #{elapsed} seconds"      
    end
    
    def multithreaded_request(request)
      threads  = []
      if num_threads == 1
        send_request(request) {|r,i|
          request.make_request(http,"#{request.body_data}#{i}") {|report|
            puts report
          }
        }
      else
        n = 0
        send_request(request) {|r,i|
          if i % num_threads == 0
            threads.each {|t| t.join}
            threads = []
          end
          threads << Thread.new(r,i) { 
            begin
            request.make_request(http,"#{request.body_data}#{i}") {|report|
              puts report unless report.empty?
            }
            rescue Object => exception
              puts exception.to_s
            end
            puts i
          }
        }
        threads.each {|t| t.join }
      end      
    end
    
  end
  
end



