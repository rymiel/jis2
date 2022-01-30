module Parser
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

    def state : State
      @states[stack.last]
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
          @symbols << Reduced(*T).new({action.name, reduction_args})
        end
      end
    end
  end
end
