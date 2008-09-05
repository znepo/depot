
log_name = ARGV[0]


times = []
File::open(log_name,"r") {|f|  
  f.each_line {|line|
    if line =~ /EXECUTED REQUEST IN: ([0-9]+\.[0-9]+)ms/
      times << $1.to_f
    end
  }  
}



def per_second(num_seconds,times)
  count  = 0.0
  reqs   = 0
  secs   = 0
  second = 0
  matrix = [[]]
  times.each {|t|
    if count <= 1000.0
      count+= t
      reqs+=1
      matrix[second] << t
    elsif secs < num_seconds
      # puts "put a total of #{reqs} requests into #{count}ms"
      secs += 1;count = 0.0;reqs = 0;second+=1
      matrix[second] = []
    end
  }
  rows = []
  i=0
  matrix.each_index {|col_idx|
    matrix[col_idx].each_index {|cell_idx|
      rows[cell_idx] ||= []
      rows[cell_idx][col_idx] = matrix[col_idx][cell_idx]
    }
  }
  [rows,matrix]
end


# secs = 100
# reqs,matrix = per_second(secs,times)
# File.open("requests-per-second.txt","w") {|f|
#   secs.times {|i| print "Second #{i+1}\t"}
#   print "\n"
#   reqs.each {|row|
#     secs.times {|cell|
#       print "#{row[cell]}\t"
#     }
#     print "\n"
#   }
# }
check_points = []
0.step(200,2) {|cp|
  check_points << cp
}
results      = {}
times.each {|t|
  check_points.each {|cp|
    if t < cp
      results[cp] ||= []
      results[cp] << t
      break
    end
  }
}


puts "ms\tpercent"
last = 0
check_points.each {|cp|
  percent = 0
  percent = (results[cp].length.to_f / times.length.to_f) * 100.0 if results[cp]
  percent = sprintf("%2.2f",percent)
  # percent = results[cp].length if results[cp]
  puts "#{cp}\t#{percent}"
  last = cp
}
