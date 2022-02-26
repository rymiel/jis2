module Parser
  abstract class IBuilder(T)
    abstract def initialize(@analysis : Analysis(self))
    abstract def singleton(entrypoint : Production, dot : Int32) : T
    abstract def initial(entrypoint : Production) : T
    abstract def closure(i : Set(T)) : Set(T)
    abstract def make_reduction(item : T, & : T -> Nil)

    def initial(entrypoint : Production) : T
      singleton(entrypoint, 0)
    end

    def final(entrypoint : Production) : T
      singleton(entrypoint, 1)
    end

    def goto(i : Set(T), x : Node) : Set(T)
      j = Set(T).new
      i.each do |item|
        if item.right_of_dot? == x
          j << item.copy_with(dot: item.dot + 1)
        end
      end
      closure(j)
    end

    def items(entrypoint : Production) : Set(Set(T))
      c = Set(Set(T)).new
      c << closure(Set{initial entrypoint})

      c.until_unchanged do
        c.each do |i|
          @analysis.all_symbols.each do |x|
            transition = goto(i, x)
            c << transition unless transition.empty?
          end
        end
      end
      c
    end
  end

  abstract struct IItem
    abstract def production : Production
    abstract def dot : Int32

    def body : Array(Node)
      @production.body
    end

    def right_of_dot? : Node?
      body[dot]?
    end

    def right_of_dot : Node
      body[dot]
    end

    def body_with_dot : Array(Node | Dot)
      body = @production.body.map(&.as(Node | Dot))
      body.insert(dot, DOT)
      body
    end
  end
end
