
class JIS2::StyleWalker
  @last = 0

  def initialize(@input : String, @out : String::Builder)
  end

  def add(sym : Symbol, r = nil, *, pos : Parser::Pos)
    w = @input[pos.s...pos.e]
    @out << @input[@last...pos.s]
    @last = pos.e
    color = if sym.in?(:module, :func, :if, :then, :else, :do, :end, :given, :until, :repeat, :finally, :return)
              Colorize::Color256.new 172
            elsif sym.in?(:_string, :_number)
              Colorize::ColorANSI::Magenta
            elsif sym == :tt_int
              Colorize::ColorANSI::Green
            elsif sym.in?(:":=", :"=", :+, :-, :*, :÷∇, :÷, :mod)
              Colorize::Color256.new 147
            elsif sym.in?(:|, :";")
              Colorize::ColorANSI::LightGray
            else
              Colorize::ColorANSI::White
            end
    @out << w.colorize color
  end

  def eof
  end
end
