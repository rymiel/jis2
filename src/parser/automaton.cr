require "any"

module Parser
  class Automaton
    alias Target = Int32 | ActionableProduction | Nil
    record State, actions : Hash(Node?, Target) do
      def initialize
        @actions = Hash(Node?, Target).new
      end

      def add_action(node : Node?, target)
        target = ActionableProduction.new target, nil if target.is_a? Production
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

    record Token, symbol : Symbol?, t : Any?
    record Reduced, name : String, t : Array(Any)

    getter states : Array(State)
    getter stack = [0]
    getter symbols = Array(Any).new
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
        in Int32
          @stack << action
          next if matched_epsilon
          @symbols << ((top.try &.t) || Any.new top)
          top = @input.shift
        in ActionableProduction
          prod = action.production
          @stack.pop prod.body.size
          reduction_args = @symbols.pop(prod.epsilon? ? 0 : prod.body.size)
          action_result = action.action.try &.call(reduction_args)
          action_result ||= Any.new Reduced.new prod.name, reduction_args
          @stack << state.actions[NonTerminal.new(prod.name)].as Int32
          @symbols << action_result
        in Nil
        end
      end
    end
  end
end
