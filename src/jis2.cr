require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

def desired(t : T, desired : T.class) : Any forall T
  Any.new t
end

def desired(t, desired : T.class) : Nil forall T
  nil
end

macro build_parser(kind, entrypoint, &block)
  {% known_types = {} of Nil => Nil %}
  ::Parser::Analysis(::Parser::{{ kind }}::Builder).build({{ entrypoint.names[0].underscore.stringify }}) do
    {% for i in block.body.expressions %}
      {% if i.is_a?(Assign) %}
        {%
          has_optional = false
          array_base = nil
          bases = [i.value]
          variants = [] of Nil
          methods = [] of Nil
          bases.each do |base|
            call_args = [] of Nil
            method = nil
            (1..50).each do # finite?
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
                  array_base = base.receiver
                  has_optional = true
                  if base.args.size > 0
                    variants << [i.target, i.target, base.args[0], base.receiver]
                    methods << nil
                    base = nil
                  else
                    base = i.target
                  end
                elsif base.name == :do
                  method = base.block
                  base = base.receiver
                  if base.is_a?(Expressions)
                    base = base.expressions[0]
                  end
                else
                  base.raise "Unknown base: #{base} (#{base.name})"
                end
              elsif !base.is_a? NilLiteral
                if base.is_a? ArrayLiteral && !base.type.nil?
                  puts "#{base.type} #{base.type.class_name} #{base}"
                  raise
                end
                call_args.unshift base
                base = nil
              end
            end
            variants << ([i.target] + call_args)
            methods << method
          end
        %}
        {% if has_optional %}
          {% target = variants[0][0] %}
          {% target = target.is_a?(Path) ? target.names[0].underscore.stringify : target %}
          {% return_type = known_types[target] %}
          {% if return_type %}
            add({{ target }}) do
              Any.new {{ return_type }}.new
            end
          {% else %}
            add({{ target }})
          {% end %}
        {% end %}
        {% for r, ri in variants %}
          {%
            r = r.map do |j|
              j.is_a?(Path) ? j.names[0].underscore.stringify : j
            end
            m = methods[ri]
            return_type = known_types[r[0]]
            can_implicit_convert = false
            if return_type && r.size == 2 && (implicit = known_types[r[1]])
              can_implicit_convert = return_type.resolve >= implicit.resolve
            end
          %}
          {% if m.nil? && array_base.nil? && !can_implicit_convert %}
            add({{ r.splat }})
          {% else %}
            add({{ r.splat }}) do |%a|
              {% found_indices = [] of Nil %}
              {% for b, bi in r %}
                {% if bi != 0 %}
                  {% if t = known_types[b] %}
                    %any = %a[{{ bi - 1 }}]
                    _{{ b.id.underscore }} = _{{ bi }} = (%any.value?({{ t }})) || (puts "ResolveFailure: #{{{ bi }}} ({{ b.id }}) of {{variants[0][0]}}, found #{%any.stored_type_name} instead"; next nil)
                    {% found_indices << bi %}
                  {% end %}
                {% end %}
              {% end %}
              {% if array_base %}
                {% if found_indices.includes?(1) && found_indices.includes?(2) %}
                  _1 << _2
                  Any.new _1
                {% else %}
                  nil
                {% end %}
              {% elsif can_implicit_convert %}
                Any.new _1
              {% elsif return_type %}
                ::desired(begin
                  {{ m.body }}
                end, {{ return_type }})
              {% else %}
                {{ m.body }}
                nil
              {% end %}
            end
          {% end %}
        {% end %}
      {% elsif i.is_a?(Call) %}
        {% if i.name == :type %}
          {% j = i.args[0] %}
          {% known_types[j.is_a?(Path) ? j.names[0].underscore.stringify : j] = i.args[1] %}
        {% end %}
      {% end %}
    {% end %}
  end
  {% debug %}
end

module JIS2
  enum PrimitiveType
    Integer
  end
  alias Type = PrimitiveType

  abstract class Statement
  end
  abstract class Expression
  end
  class Declaration < Expression
    def initialize(@type : TypeName, @value : Expression)
    end
  end
  class DecimalLiteral < Expression
    def initialize(@value : Int64)
    end
  end
  record TypeName, type : Type, name : String
  class Module
    def initialize(@name : String)
    end
  end
end

states = build_parser(LR1, Program) do
  type :string,       String
  type :word,         String
  type :number,       Int64
  type TypeSpecifier, JIS2::Type
  type Program,       JIS2::Module
  type Module,        JIS2::Module
  type Statements,    Array(JIS2::Statement)
  type Statement,     JIS2::Statement
  type Expressions,    Array(JIS2::Expression)
  type Expression,     JIS2::Expression
  type DefinitionArg, JIS2::TypeName
  type DecimalLiteral, JIS2::DecimalLiteral
  type Declaration, JIS2::Declaration

  Program         = Module.do { _module }
  Module          = (:r_module + :string + Statements + :r_end).do { JIS2::Module.new _string }
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
  DecimalLiteral  = :number.do { JIS2::DecimalLiteral.new _number }
  Declaration     = (DefinitionArg + :assign + Expression).do { JIS2::Declaration.new _definition_arg, _expression }
  Assignment      = :word + :assign + Expression
  Call            = CallName + :sq_l + CallArgs + :sq_r
  CallName        = :word | :plus | :eq
  CallArgs        = Expression[:pipe]
  TypeSpecifier   = (:tt_int).do { JIS2::PrimitiveType::Integer }
  FunctionDef     = TypeSpecifier + :r_func + :word + :sq_l + DefinitionArgs + :sq_r + Statements + :r_end
  DefinitionArgs  = DefinitionArg[:pipe]
  DefinitionArg   = (TypeSpecifier + :word).do { JIS2::TypeName.new _type_specifier, _word }
end
at = Parser::Automaton.new(states)

class TreeWalker(*T)
  record Token(*T), symbol : Symbol?, t : Array(Union(Any, Token(*T), Reduced(*T), *T)?) do
    def inspect(io : IO)
      io << "<" << @symbol.colorize.cyan
      u = @t[0]
      if u.is_a?(Any)
        io << ":" << u.not_nil!.stored_type_name.colorize.dark_gray
        io << " " << u
      elsif !u.nil?
        io << " " << u
      end
      io << ">"
    end
  end
  record Reduced(*T), name : String, t : Array(Union(Any, Token(*T), Reduced(*T), *T)) do
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
  alias AReduced = Parser::Automaton::Reduced
  alias AToken = Parser::Automaton::Token
  def walk(any : Any) : Union(Any, Token(*T), Reduced(*T), *T)
    {% begin %}
      if r = any.value?(AReduced)
        Reduced(*T).new r.name, r.t.map { |i| self.walk(i).as(Union(Any, Token(*T), Reduced(*T), *T)) }
      elsif t = any.value?(AToken)
        Token(*T).new t.symbol, [t.t.try { |i| self.walk(i).as(Union(Any, Token(*T), Reduced(*T), *T)) }]
      {% for i in @type.type_vars[0] %}
        {% check_type = i.resolve %}
        elsif %i = any.value?({{ check_type }})
          %i
      {% end %}
      else
        any
      end
    {% end %}
  end
end

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1i64) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
PrettyPrint.format(TreeWalker(Array(JIS2::Statement), JIS2::TypeName, JIS2::Declaration).new.walk(tree), STDOUT, 119)
puts
