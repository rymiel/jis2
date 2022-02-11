
class String
  ANSI_REGEX = /\x1b\[[0-9;]*m/
  def strip_color : String
    self.gsub(ANSI_REGEX, "")
  end
  def color_ljust(n : Int32) : String
    self.ljust(n + (self.size - strip_color.size))
  end
end

module Enumerable(T)
  def until_unchanged(&)
    old_size = self.size
    ret = nil
    loop do
      ret = yield
      break if self.size == old_size
      old_size = self.size
    end
    ret
  end
end

module Parser
  struct Ref(T)
    @t : Slice(T)
    def initialize(t : T)
      @t = Slice(T).new(1, t)
    end

    def value : T
      @t[0]
    end

    def to_s(io : IO)
      value.to_s io
    end

    def inspect(io : IO)
      io << "&"
      value.inspect io
    end

    def pretty_print(pp)
      pp.text "&"
      value.pretty_print pp
    end
  end

  struct Opt(T)
    @t : T?
    def initialize(t : T)
      @t = t
    end

    def initialize
      @t = nil
    end

    def value : T?
      @t
    end
  end
end
