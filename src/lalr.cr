require "colorize"

class String
  ANSI_REGEX = /\x1b\[[0-9;]*m/
  def strip_color : String
    self.gsub(ANSI_REGEX, "")
  end
  def color_ljust(n : Int32) : String
    self.ljust(n + (self.size - strip_color.size))
  end
end

module Parser
  VERSION = "0.1.0"

  abstract struct Node
  end
  record NonTerminal < Node, name : String do
    def inspect(io : IO)
      io << name.inspect.colorize.yellow
    end
  end
  record Terminal < Node, symbol : Symbol do
    def inspect(io : IO)
      io << symbol.inspect.colorize.cyan
    end
  end
  record Dot do
    def inspect(io : IO)
      io << "·".colorize.red.bold
    end
  end
  record Epsilon < Node do
    def inspect(io : IO)
      io << "ɛ".colorize.red.bold
    end
  end
  record EndOfStream < Node do
    def inspect(io : IO)
      io << "$".colorize.red.bold
    end
  end
  DOT = Dot.new
  EPSILON = Epsilon.new
  EOS = EndOfStream.new
  ENTRYPOINT = "ENTRYPOINT"

  record Production, name : String, body : Array(Node) do
    def inspect(io : IO)
      io << smart_name << " -> " << @body.join " "
    end
    def smart_name : String
      (name == ENTRYPOINT ? "*".colorize.bold.green : name.colorize.bold).to_s
    end
    def dot_index : Int32
      @body.index(DOT).not_nil!
    end
    def epsilon? : Bool
      @body == [EPSILON]
    end
    def result : NonTerminal
      NonTerminal.new @name
    end
  end

  abstract class IBuilder(T)
    abstract def initialize(analysis : Analysis(self))
    abstract def closure(i : Set(T)) : Set(T)
    abstract def goto(i : Set(T), x : Node) : Set(T)
    abstract def items(entrypoint : Production) : Set(Set(T))
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

  module LR0
    record Item < IItem, production : Production, dot : Int32 do
      def inspect(io : IO)
        io << "[" << @production.smart_name << " -> " << body_with_dot.join(" ") << "]"
      end
    end
    class Builder < IBuilder(Item)
      def initialize(@analysis : Analysis(self))
      end

      def final(e)
        Item.new(e, 1)
      end

      def closure(i : Set(Item)) : Set(Item)
        j = i.dup
        loop do
          done = true
          j.each do |item|
            if (r = item.right_of_dot?).is_a? NonTerminal
              @analysis.@rules.select(&.name.== r.name).each do |production|
                item = Item.new production, 0
                unless item.in?(j)
                  done = false
                  j << item
                end
              end
            end
          end
          break if done
        end
        j
      end

      def goto(i : Set(Item), x : Node) : Set(Item)
        j = Set(Item).new
        i.each do |item|
          if item.right_of_dot? == x
            shifted = item.copy_with dot: item.dot + 1
            j += closure(Set{shifted})
          end
        end
        j
      end

      def items(entrypoint : Production) : Set(Set(Item))
        c = Set(Set(Item)).new
        c << closure(Set{Item.new entrypoint, 0})

        loop do
          done = true
          c.each do |i|
            @analysis.all_symbols.each do |x|
              transition = goto(i, x)
              if !transition.empty? && !transition.in?(c)
                c << transition
                done = false
              end
            end
          end
          break if done
        end
        c
      end
    end
  end

  module LR1
    record Item < IItem, production : Production, dot : Int32, lookahead : Node do
      def inspect(io : IO)
        io << "[" << @production.smart_name << " -> " << body_with_dot.join(" ") << "; " << lookahead << "]"
      end
    end
    class Builder < IBuilder(Item)
      def initialize(@analysis : Analysis(self))
      end

      def final(e)
        Item.new(e, 1, EOS)
      end

      def closure(i : Set(Item)) : Set(Item)
        j = i.dup
        loop do
          done = true
          j.each do |item|
            if (r = item.right_of_dot?).is_a? NonTerminal
              r2 = item.body[item.dot + 1]?
              @analysis.@rules.select(&.name.== r.name).each do |production|
                compound_first = if r2.nil?
                  @analysis.first(item.lookahead)
                else
                  @analysis.first(r2, item.lookahead)
                end
                compound_first.each do |b|
                  item = Item.new production, 0, b
                  unless item.in?(j)
                    done = false
                    j << item
                  end
                end
              end
            end
          end
          break if done
        end
        j
      end

      def goto(i : Set(Item), x : Node) : Set(Item)
        j = Set(Item).new
        i.each do |item|
          if item.right_of_dot? == x
            shifted = item.copy_with dot: item.dot + 1
            j += Set{shifted}
          end
        end
        closure(j)
      end

      def items(entrypoint : Production) : Set(Set(Item))
        c = Set(Set(Item)).new
        c << closure(Set{Item.new entrypoint, 0, EOS})

        loop do
          done = true
          c.each do |i|
            @analysis.all_symbols.each do |x|
              transition = goto(i, x)
              if !transition.empty? && !transition.in?(c)
                c << transition
                done = false
              end
            end
          end
          break if done
        end
        c
      end
    end
  end

  class Analysis(T)

    @first = Hash(Node, Set(Node)).new do |hash, key|
      hash[key] = Set(Node).new
    end
    @follow = Hash(NonTerminal, Set(Node)).new do |hash, key|
      hash[key] = Set(Node).new
    end
    getter rules = Array(Production).new
    getter first, follow

    def initialize
      @builder = uninitialized T
      @builder = T.new self
    end

    def all_symbols : Set(Node)
      @rules.map(&.result).to_set + @rules.map(&.body).flatten.to_set
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
      c = @builder.items(e)
      states = Array(Automaton::State).new
      c.each_with_index do |i, j|
        #~# puts "#{j.to_s.rjust 3}. #{i.join("\n     ")}"
        state = Automaton::State.new
        states << state
        #~# actions = {} of Node => String
        #~# gotos = {} of NonTerminal => String
        i.each do |item|
          if item == @builder.final(e)
            state.add_action nil, nil
            #~# puts "Accept!".colorize.green.bold
            next
          end
          right_of_dot = item.body[item.dot]?
          if right_of_dot.nil?
            left_of_dot = item.body[item.dot - 1]?
            unless left_of_dot.nil?
              unless item.production.result == e.result
                @follow[item.production.result].each do |a|
                  state.add_action a, item.production
                  #~# actions[a] = "Reduce #{item.production}"
                end
              end
            end
          else
            if right_of_dot.is_a? Terminal || right_of_dot == EPSILON
              transitioned_state = @builder.goto(i, right_of_dot)
              if transition_index = c.index(transitioned_state)
                state.add_action right_of_dot, transition_index
                #~# actions[right_of_dot] = "State #{transition_index.to_s.rjust 3}."
              end
            end
          end
          all_symbols.select(NonTerminal).each do |k|
            transitioned_state = @builder.goto(i, k)
            if transition_index = c.index(transitioned_state)
              state.add_action k, transition_index
              #~# gotos[k] = "State #{transition_index.to_s.rjust 3}."
            end
          end
        end
        #~# print actions.map { |ki, kj| "Action - upon #{ki.to_s.color_ljust 32} => #{kj}\n" }.join ""
        #~# print gotos.map { |ki, kj| "  Goto - upon #{ki.to_s.color_ljust 32} => #{kj}\n" }.join ""
        #~# puts "\n--"
        #~# puts
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

    def populate_first
      @first[EOS] = Set(Node){EOS}
      loop do
        done = true
        all_symbols.each do |x|
          s = case x
          when Terminal then Set(Node){x}
          when Production then x.body.epsilon? ? Set(Node){EPSILON} : Set(Node).new
          when NonTerminal
            running = @first[x].dup
            @rules.select(&.name.== x.name).each do |candidate|
              running += first(candidate.body.reject(DOT))
              running << EPSILON if candidate.epsilon?
            end
            running
          else Set(Node).new
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

  record Token(T), symbol : Symbol, object : T

  def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
    {symbol, t}
  end

  def self.tok(symbol : Symbol) : {Symbol, Nil}
    {symbol, nil}
  end

  class Automaton(*T)
    alias Target = Int32 | Production | Nil
    record State, actions : Hash(Node?, Target) do
      def initialize
        @actions = Hash(Node?, Target).new
      end

      def add_action(node : Node?, target)
        if @actions.has_key?(node)
          if @actions[node] != (target.as Target)
            raise "Conflict on #{node} action: #{@actions[node]} vs #{target}"
          end
        end
        @actions[node] = target.as Target
      end
    end

    def initialize(@states)
    end

    record Token(*T), t : {Symbol?, Union(Nil, *T)} do
      def inspect(io : IO)
        io << "<" << t[0].colorize.cyan
        unless (u = t[1]).nil?
          io << " " << u
        end
        io << ">"
      end
    end
    record Reduced(*T), t : {String, Array(Token(*T) | Reduced(*T))} do
      def inspect(io : IO)
        io << t[0].colorize.yellow << "(" << t[1].join(", ") << ")"
      end
      def pretty_print(pp)
        pp.text t[0].colorize.yellow.to_s
        pp.surround(" {", "}", "", nil) do
          t[1].each_with_index do |elem, i|
            pp.comma if i > 0
            elem.pretty_print(pp)
          end
        end
      end
    end

    getter states : Array(State)
    getter stack = [0]
    getter symbols = Array(Token(*T) | Reduced(*T)).new
    @input = Array(Token(*T)).new

    def state_num : Int32
      stack.last
    end

    def state : State
      @states[state_num]
    end

    def add_state
      new_state = State.new(Hash(Node?, Target).new)
      @states << new_state
      new_state
    end

    def <<(token : {Symbol, U}) forall U
      @input << Token(*T).new token
      self
    end

    def <<(token : Symbol)
      @input << Token(*T).new({token, nil})
      self
    end

    def run
      @input << Token(*T).new({nil, nil})
      top = @input.shift
      loop do
        a = top.t[0].nil? ? EOS : Terminal.new(top.t[0].not_nil!)
        puts "Run: State nr. #{state_num}: #{state.actions}"
        if a == EOS && state.actions.has_key?(nil)
          return @symbols.pop
        end
        matched_epsilon = false
        action = state.actions[a]?
        if action.nil?
          action = state.actions[EPSILON]?
          action = state.actions[a] if action.nil?
          matched_epsilon = true
        end
        puts "Input: #{top} => action #{action}"
        case action
        when Int32
          @stack << action
          @symbols << top unless matched_epsilon
          puts "Shifted: #{@stack}"
          puts "Symbols: #{@symbols}"
          top = @input.shift unless matched_epsilon
        when Production
          @stack.pop action.body.size
          # reduction_args = @symbols.pop(action.epsilon? ? 0 : action.body.size)
          reduction_args = @symbols.pop(action.body.size)
          puts "Popped for reducing to #{action.name}: #{reduction_args}"
          reduction_args.clear if action.epsilon?
          @stack << state.actions[NonTerminal.new(action.name)].as Int32
          @symbols << Reduced(*T).new({action.name, reduction_args})
          puts "Reduced: #{@stack}"
          puts "Symbols: #{@symbols}"
        end
      end
    end
  end

  alias LR0Analysis = Analysis(LR0::Builder)
  alias LR1Analysis = Analysis(LR1::Builder)

  # puts "\n\n== LR(0) ==\n\n"
  # gen = LR0Analysis.new
  # gen.add("S", "C", "C")
  # gen.add("C", :c, "C")
  # gen.add("C", :d)
  # gen.initial("S")

  # gen.add("E", "E", :"+", "T")
  # gen.add("E", "T")
  # gen.add("T", "T", :"*", "F")
  # gen.add("T", "F")
  # gen.add("F", :"(", "E", :")")
  # gen.add("F", :id)
  # gen.initial("E")

  puts "\n\n== LR(1) ==\n\n"
  gen1 = LR1Analysis.new
  # gen1.add("S", "C", "C")
  # gen1.add("C", :c, "C")
  # gen1.add("C", :d)
  # gen1.initial("S")

  gen1.add("program", "module")
  gen1.add("module", :r_module, :string, "statements", :r_end)
  gen1.add("statements")
  gen1.add("statements", "statements", "statement")
  gen1.add("statement", "function_definition")
  gen1.add("statement", :r_return, "expression", :semi)
  gen1.add("statement", "expression", :semi)
  gen1.add("statement", "block")
  gen1.add("block", "block_statements", :r_do, "statements", "block_finally", :r_end)
  gen1.add("block_statements")
  gen1.add("block_statements", "block_statements", "block_statement")
  gen1.add("block_finally")
  gen1.add("block_finally", :r_finally, "statement")
  gen1.add("block_statement", :r_given, "expression")
  gen1.add("block_statement", :r_repeat, "expression")
  gen1.add("block_statement", :r_until, "expression")
  gen1.add("expression", "decimal_literal")
  gen1.add("expression", :word)
  gen1.add("expression", "declaration")
  gen1.add("expression", "assignment")
  gen1.add("expression", "call")
  gen1.add("decimal_literal", :number)
  gen1.add("declaration", "definition_arg", :assign, "expression")
  gen1.add("assignment", :word, :assign, "expression")
  gen1.add("call", "call_name", :sq_l, "call_args", :sq_r)
  gen1.add("call_name", :word)
  gen1.add("call_name", :plus)
  gen1.add("call_name", :eq)
  gen1.add("call_args")
  gen1.add("call_args", "expression")
  gen1.add("call_args", "call_args", :pipe, "expression")
  gen1.add("type_specifier", :tt_int)
  gen1.add("function_definition", "type_specifier", :r_func, :word, :sq_l, "definition_args", :sq_r, "statements", :r_end)
  gen1.add("definition_args")
  gen1.add("definition_args", "definition_arg")
  gen1.add("definition_args", "definition_args", :pipe, "definition_arg")
  gen1.add("definition_arg", "type_specifier", :word)
  states = gen1.build("program")
  at = Automaton(String, Int32).new(states)

  at << :r_module << tok(:string, "default")
    at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
      at << :tt_int << tok(:word, "num")
    at << :sq_r
      at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1) << :r_do << :r_end
    at << :r_end
  at << :r_end
  tree = at.run
  pp tree

end
