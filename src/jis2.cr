require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

macro build_parser(kind, entrypoint, &block)
  ::Parser::Analysis(::Parser::{{ kind }}::Builder).build({{ entrypoint }}) do
    {% for i in block.body.expressions %}
      {% if i.is_a?(Assign) %}
        {%
          has_optional = false
          bases = [i.value]
          variants = [] of Nil
          bases.each do |base|
            call_args = [] of Nil
            (1..100).each do # finite?
              if base.is_a? Call
                if base.name == :+
                  call_args.unshift base.args[0]
                  base = base.receiver
                elsif base.name == :optional
                  has_optional = true
                  base = base.args[0]
                elsif base.name == :|
                  bases << base.args[0]
                  base = base.receiver
                elsif base.name == :[]
                  call_args.unshift base.receiver
                  has_optional = true
                  if base.args.size > 0
                    variants << [i.target, i.target, base.args[0], base.receiver]
                    base = nil
                  else
                    base = i.target
                  end
                else
                  base.raise "Unknown base: #{base} (#{base.name})"
                end
              elsif !base.is_a? NilLiteral
                call_args.unshift base
                base = nil
              end
            end
            variants << ([i.target] + call_args)
          end
        %}
        {% if has_optional %}
          {% target = variants[0][0] %}
          {% target = target.is_a?(Path) ? target.names[0].underscore.stringify : target %}
          add({{ target }})
        {% end %}
        {% for r in variants %}
          {% r = r.map do |j|
            if j.is_a? Path
              j.names[0].underscore.stringify
            else
              j
            end
          end %}
          add({{ r.splat }})
        {% end %}
      {% end %}
    {% end %}
  end
  {% debug %}
end

states = build_parser(LR0, "program") do
  Program         = Module
  Module          = :r_module + :string + Statements + :r_end
  Statements      = Statement[]
  Statement       = FunctionDef \
                  | :r_return + Expression + :semi \
                  | Expression + :semi \
                  | Block
  Block           = BlockStatements + :r_do + Statements + BlockFinally + :r_end
  BlockStatements = BlockStatement[]
  BlockFinally    = optional(:r_finally + Statement)
  BlockStatement  = :r_given + Expression \
                  | :r_repeat + Expression \
                  | :r_until + Expression
  Expression      = DecimalLiteral | :word | Declaration | Assignment | Call
  DecimalLiteral  = :number
  Declaration     = DefinitionArg + :assign + Expression
  Assignment      = :word + :assign + Expression
  Call            = CallName + :sq_l + CallArgs + :sq_r
  CallName        = :word | :plus | :eq
  CallArgs        = Expression[:pipe]
  TypeSpecifier   = :tt_int
  FunctionDef     = TypeSpecifier + :r_func + :word + :sq_l + DefinitionArgs + :sq_r + Statements + :r_end
  DefinitionArgs  = DefinitionArg[:pipe]
  DefinitionArg   = TypeSpecifier + :word
end
at = Parser::Automaton.new(states)

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
pp tree
