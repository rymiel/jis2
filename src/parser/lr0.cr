require "./interfaces"

module Parser::LR0
  record Item < IItem, production : Production, dot : Int32 do
    def inspect(io : IO)
      io << "[" << @production.smart_name << " -> " << body_with_dot.join(" ") << "]"
    end
  end
  class Builder < IBuilder(Item)
    def initialize(@analysis : Analysis(self))
    end

    def singleton(e : Production, dot : Int32) : Item
      Item.new e, dot
    end

    def make_reduction(item : Item, &)
      @analysis.@follow[item.production.result].each do |a|
        yield a
      end
    end

    def closure(i : Set(Item)) : Set(Item)
      j = i.dup
      j.until_unchanged do
        j.each do |item|
          if (r = item.right_of_dot?).is_a? NonTerminal
            @analysis.@rules.map(&.production).select(&.name.== r.name).each do |production|
              j << Item.new production, 0
            end
          end
        end
      end
      j
    end
  end
end
