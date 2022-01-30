require "../any"

module Parser
  class Automaton
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

    record Token, symbol : Symbol?, t : Any? do
      def inspect(io : IO)
        io << "<" << @symbol.colorize.cyan
        unless @t.nil?
          io << ":" << @t.not_nil!.@stored_type_name.colorize.dark_gray
          io << " " << @t
        end
        io << ">"
      end
    end
    record Reduced, name : String, t : Array(Token | Reduced) do
      def inspect(io : IO)
        io << @name.colorize.yellow << "(" << @t.join(", ") << ")"
      end
      def pretty_print(pp)
        pp.text @name.colorize.yellow.to_s
        pp.surround(" {", "}", "", nil) do
          @t.each_with_index do |elem, i|
            pp.comma if i > 0
            elem.pretty_print(pp)
          end
        end
      end
    end

    getter states : Array(State)
    getter stack = [0]
    getter symbols = Array(Token | Reduced).new
    @input = Array(Token).new

    def state : State
      @states[stack.last]
    end

    def add_state
      new_state = State.new(Hash(Node?, Target).new)
      @states << new_state
      new_state
    end

    def <<(token : {Symbol, U}) forall U
      @input << Token.new token[0], Any.new token[1]
      self
    end

    def <<(token : Symbol)
      @input << Token.new token, nil
      self
    end

    def run
      @input << Token.new nil, nil
      top = @input.shift
      loop do
        a = top.symbol.nil? ? EOS : Terminal.new(top.symbol.not_nil!)
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
          next if matched_epsilon
          @symbols << top
          top = @input.shift
        when Production
          @stack.pop action.body.size
          reduction_args = @symbols.pop(action.body.size)
          reduction_args.clear if action.epsilon?
          @stack << state.actions[NonTerminal.new(action.name)].as Int32
          @symbols << Reduced.new action.name, reduction_args
        end
      end
    end
  end
end
