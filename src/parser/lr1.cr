require "./interfaces"

module Parser::LR1
  record Item < IItem, production : Production, dot : Int32, lookahead : Node do
    def inspect(io : IO)
      io << "[" << @production.smart_name << " -> " << body_with_dot.join(" ") << "; " << lookahead << "]"
    end
  end
  class Builder < IBuilder(Item)
    def initialize(@analysis : Analysis(self))
    end

    def singleton(e : Production, dot : Int32) : Item
      Item.new e, dot, EOS
    end

    def make_reduction(item : Item, &)
      yield item.lookahead
    end

    def closure(i : Set(Item)) : Set(Item)
      j = i.dup
      j.until_unchanged do
        j.each do |item|
          if (r = item.right_of_dot?).is_a? NonTerminal
            r2 = item.body[item.dot + 1]?
            @analysis.@rules.map(&.production).select(&.name.== r.name).each do |production|
              compound_first = @analysis.first(r2, item.lookahead)
              compound_first.each do |b|
                j << Item.new production, 0, b
              end
            end
          end
        end
      end
      j
    end
  end
end
