module Parser


  class Analysis(T)
    @first = Hash(Node, Set(Node)).new { |hash, key| hash[key] = Set(Node).new }
    @follow = Hash(NonTerminal, Set(Node)).new { |hash, key| hash[key] = Set(Node).new }
    getter rules = Array(Production).new
    getter first, follow

    class ProductionBuilder(T)
      @name : String
      @analysis : Analysis(T)
      @body = Array(Node).new
      def initialize(@analysis, @name)
      end

      def line(*items)
        @analysis.add(@name, *items)
      end

      def empty
        line
      end
    end

    def initialize
      @builder = uninitialized T
      @builder = T.new self
    end

    def self.build(entrypoint : String, &) : Array(Automaton::State)
      instance = new
      with instance yield
      instance.build entrypoint
    end

    def all_symbols : Set(Node)
      @rules.map(&.result).to_set + @rules.map(&.body).flatten.to_set
    end

    def add(name : String, &)
      builder = ProductionBuilder(T).new self, name
      with builder yield
    end

    def add(name : String, *items) : Production
      mapped = items.to_a.map { |i|
        x = case i
        when String then NonTerminal.new i
        when Symbol then Terminal.new i
        else raise "Invalid rule item #{i.class}"
        end
        x.as Node
      }
      mapped = [EPSILON] of Node if mapped.empty?
      production = Production.new name, mapped
      @rules << production
      production
    end

    def build(entrypoint : String) : Array(Automaton::State)
      e = add(ENTRYPOINT, entrypoint)

      populate_first
      populate_follow(e.result)
      states = Array(Automaton::State).new

      c = @builder.items(e)
      c.each_with_index do |i, j|
        state = Automaton::State.new
        states << state

        apply_goto = ->(node : Node) {
          if transition_index = c.index @builder.goto(i, node)
            state.add_action node, transition_index
          end
        }

        i.each do |item|
          state.add_action(nil, nil) if item == @builder.final(e)
          if (right_of_dot = item.right_of_dot?).nil?
            left_of_dot = item.body[item.dot - 1]?
            unless left_of_dot.nil?
              unless item.production.result == e.result
                @follow[item.production.result].each do |a|
                  state.add_action a, item.production
                end
              end
            end
          elsif right_of_dot.is_a? Terminal || right_of_dot == EPSILON
            apply_goto.call(right_of_dot)
          end
          all_symbols.select(NonTerminal).each do |k|
            apply_goto.call(k)
          end
        end
      end
      states
    end

    def first(x : Iterable(Node)) : Set(Node)
      s = Set(Node).new
      all_epsilon = true
      x.each do |i|
        i_set = @first[i]
        s += (i_set - {EPSILON})
        unless EPSILON.in?(i_set)
          all_epsilon = false
          break
        end
      end
      s << EPSILON if all_epsilon
      s
    end

    def first(*x : Node) : Set(Node)
      first x
    end

    def first(*x : Node?) : Set(Node)
      first x.to_a.compact
    end

    def populate_first
      @first[EOS] = Set(Node){EOS}
      loop do
        done = true
        all_symbols.each do |x|
          s = case x
          in Terminal then Set(Node){x}
          in NonTerminal
            running = @first[x].dup
            @rules.select(&.name.== x.name).each do |candidate|
              running += first(candidate.body.reject(DOT))
              running << EPSILON if candidate.epsilon?
            end
            running
          in Node then Set(Node).new
          end
          done = false if s != @first[x]
          @first[x] = s
        end
        break if done
      end
    end

    def populate_follow(entrypoint : NonTerminal)
      loop do
        done = true
        all_symbols.select(NonTerminal).each do |x|
          running = @follow[x].dup
          running << EOS if x == entrypoint
          @rules.each do |i|
            body = i.body.reject(DOT)
            self_index = body.index x
            next if self_index.nil?

            following = body[self_index + 1]?
            following_epsilon = !following.nil? && @first[following].includes?(EPSILON)

            running += (@first[following] - {EPSILON}) unless following.nil?
            running += @follow[i.result] if following.nil? || following_epsilon
          end
          done = false if running != @follow[x]
          @follow[x] = running
        end
        break if done
      end
    end
  end
end
