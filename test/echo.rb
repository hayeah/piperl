TRACE = File.new("/dev/pts/2","w")

def trace(data)
  TRACE.puts "(#{Process.pid}) #{data}"
end

class Piperl
  def initialize(i,o)
    @i = i
    @o = o
  end

  def each
    while data = self.read
      data = yield(data)
      write(data)
    end
  end
  
  def read
    size = ""
    while true
      c = @i.getc
      break unless (?0..?9).include?(c)
      size << c
    end
    exit(0) unless c
    @i.ungetc(c)
    
    expect ?~
    
    size = size.to_i
    data = ""
    while size > 0
      str = @i.read(size)
      if str == nil
        # pipe closed, just exit
        exit(0)
        # raise EOFError, "End of file reached"
      end
      size -= str.length
      data << str
    end

    expect ?~
    expect ?$
    trace "read: #{data}"
    return data
  end

  def expect(c)
    raise unless c == @i.getc
  end

  def write(data)
    data = "#{Process.pid}==#{data.to_s}"
    trace "write: #{data}"
    @o << data.size
    @o.putc ?~
    @o << data
    @o.putc ?~
    @o.putc ?$
    @o.flush
  end
end

i = 0
piperl = Piperl.new($stdin,$stdout)
piperl.each do |data|
  case data
  when "fail"
    Kernel.exit(0)
  when "fail-after"
    piperl.write("swan-song")
    Kernel.exit(0)
  when "timeout"
    sleep(10000) # go to sleep, baby
  else
    data # echo
  end
end
