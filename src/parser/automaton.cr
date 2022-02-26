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

    record RowCol, row : Int32, col : Int32
    record RowPos, s : RowCol, e : RowCol
    record Token, symbol : Symbol?, t : Any?, pos : ChrPos? do
      def inspect(io : IO) : Nil
        io << "Token"
        if symbol.nil?
          io << ":EOF"
        else
          symbol.inspect io
        end
        unless t.nil?
          io << "("
          t.inspect io
          io << ")"
        end
        io << "@" << (pos || "???")
      end
    end
    record Reduced, name : String, t : Array(StackSym)

    getter states : Array(State)
    getter stack = [0]
    getter symbols = Array(StackSym).new

    def state : State
      @states[stack.last]
    end

    def add_state
      new_state = State.new(Hash(Node?, Target).new)
      @states << new_state
      new_state
    end

    def add(token : Symbol, value : U, *, pos : Pos? = nil) forall U
      run Token.new token, Any.new(value), pos
      self
    end

    def add(token : Symbol, *, pos : ChrPos? = nil)
      run Token.new token, nil, pos
      self
    end

    def eof
      run Token.new nil, nil, nil
    end

    def run(top : Token)
      a = top.symbol.nil? ? EOS : Terminal.new(top.symbol.not_nil!)
      if a == EOS && state.actions.has_key?(nil)
        return @symbols.pop
      end
      matched_epsilon = false
      action = state.actions[a]?
      if action.nil?
        action = state.actions[EPSILON]?
        raise "No action #{a} or epsilon in #{state}" if action.nil?
        matched_epsilon = true
      end
      puts "Input: #{top} => action #{action}"
      case action
      in Int32
        @stack << action
        return run(top) if matched_epsilon
        if (t = top.try &.t).nil?
          @symbols << StackSym.new Any.new(top), top.pos
        else
          @symbols << StackSym.new t, top.pos
        end
        return
      in ActionableProduction
        prod = action.production
        @stack.pop prod.body.size
        reduction_args = @symbols.pop(prod.epsilon? ? 0 : prod.body.size)
        action_result = action.action.try &.call(reduction_args)
        action_result ||= StackSym.new Any.new(Reduced.new prod.name, reduction_args), nil
        @stack << state.actions[NonTerminal.new(prod.name_idx)].as Int32
        @symbols << action_result
      in Nil
      end
      run top
    end
  end
end
