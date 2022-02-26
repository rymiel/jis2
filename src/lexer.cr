require "./parser/automaton"

class Lexer(*T)
  @matches = Array({Regex, Symbol, Proc(Regex::MatchData, Union(*T))?}).new
  @skips = Array(Regex).new

  def self.build(&) : self
    ins = new
    with ins yield
    ins
  end

  def match(regex : Regex, result : Symbol)
    @matches << {regex, result, nil}
  end

  def match(*symbols : Symbol)
    symbols.each do |symbol|
      @matches << {Regex.new(Regex.escape symbol.to_s), symbol, nil}
    end
  end

  macro keywords(*kws)
    {% for kw in kws %}
      match(Regex.new(Regex.escape({{kw}}.to_s)), {{"kw_#{kw.id}".id.symbolize}})
    {% end %}
  end

  def match(regex : Regex, symbol : Symbol, &result : Regex::MatchData -> Union(*T))
    @matches << {regex, symbol, result}
  end

  def skip(regex : Regex)
    @skips << regex
  end

  def lex(input : String, at : Parser::Automaton)
    pos = 0
    until pos == input.size
      found = false
      next if @skips.each do |m|
        if match = m.match(input, pos, Regex::Options::ANCHORED)
          pos = match.end
          break true
        end
      end
      next if @matches.each do |m, sym, r|
        if match = m.match(input, pos, Regex::Options::ANCHORED)
          r = r.call match if r.is_a? Proc
          pos = Parser::ChrPos.new match.begin, match.end
          if r.nil?
            at.add sym, pos: pos
          else
            at.add sym, r, pos: pos
          end
          pos = match.end
          break true
        end
      end
      next if found
      raise "no match at pos #{pos}: `#{input[pos..pos+30].dump_unquoted}`..."
    end
    at.eof
  end
end
