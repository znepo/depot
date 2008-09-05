require 'test-framework/runner'

host = ARGV[0] || "localhost"
port = ARGV[1] || 2323

network_suite = DepotTest::Runner.new("network-tests", host, port)
network_suite.num_threads = 32
network_suite.run

input_suite = DepotTest::Runner.new("input-tests", host, port)
input_suite.num_threads = 2
input_suite.run

