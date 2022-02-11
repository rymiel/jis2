require "./parser"

def self.tok(symbol : Symbol, t : T) : {Symbol, T} forall T
  {symbol, t}
end

struct Ref(T)
  @t : Slice(T)
  def initialize(t : T)
    @t = Slice(T).new(1, t)
  end

  def value : T
    @t[0]
  end

  def to_s(io : IO)
    value.to_s io
  end

  def inspect(io : IO)
    io << "&"
    value.inspect io
  end
end

struct Opt(T)
  @t : T?
  def initialize(t : T)
    @t = t
  end

  def initialize
    @t = nil
  end

  def value : T?
    @t
  end
end

module Node
  annotation RuleNode
  end
  annotation Aliased
  end
  annotation Separator
  end
  annotation EnumVal
  end

  class ResolveFailureError < TypeCastError
    def initialize(@message)
    end
  end

  macro structure(*t, given_target = nil)
    {% for i in t %}
      {% if i.is_a? Call %}
        @[::Node::RuleNode(obj: {{ i.receiver }}, target: {{ i.args[0] }})]
      {% else %}
        @[::Node::RuleNode(obj: {{ i }})]
      {% end %}
    {% end %}
    def self._structure%structure{t}
    end
  end

  def self.compose_known_rules(builder : ::Parser::Analysis(T)) forall T
    {% begin %}
      {%
        spider = [] of Nil
        ::Node.includers.each do |i|
          if i.abstract?
            i.subclasses.each do |j|
              if ::Node.includers.includes?(j)
                spider << {i, [i, j], [{0, "subclass".id, j}], :alias}
              end
            end
          end
          i.class.methods.each do |m|
            if m.name.starts_with? "_structure"
              rule_args = [i]
              resolves = [] of Nil
              m.annotations(::Node::RuleNode).each_with_index do |a, j|
                rule_args << a[:obj]
                if a[:target]
                  target = i.instance_vars.find(&.name.== a[:target].name[1..])
                  target_type = target.type
                  target_name = target.name
                  if target_type.union?
                    union_types = target_type.union_types
                    if union_types.includes? ::Nil
                      base_type = union_types.find(&.!= ::Nil)
                      spider << {base_type, [base_type, base_type], [
                        {0, "optional".id, base_type}
                      ], :optional}
                      spider << {base_type, [base_type], [] of Nil, :optional}
                    end
                  end
                  if target_type <= ::Array
                    member = target_type.type_vars[0]
                    if separator_ann = target.annotation(::Node::Separator)
                      separator = separator_ann[0]
                      unique_name = separator_ann[:name] || target_type
                      spider << {target_type, [unique_name], [] of Nil}
                      spider << {target_type, [unique_name, member], [
                        {0, "array appended element".id, member}
                      ]}
                      spider << {target_type, [unique_name, unique_name, separator, member], [
                        {0, "array base".id, target_type},
                        {2, "array appended element".id, member}
                      ]}
                    else
                      spider << {target_type, [target_type], [] of Nil}
                      spider << {target_type, [target_type, target_type, member], [
                        {0, "array base".id, target_type},
                        {1, "array appended element".id, member}
                      ]}
                    end
                  elsif target_type <= ::Enum
                    if enum_ann = target_type.annotation(::Node::EnumVal)
                      ann_keys = enum_ann.named_args.keys
                      unmatched = target_type.constants.map(&.underscore).reject { |h| ann_keys.includes? h }
                      target.raise "EnumVal annotation is missing values for the enum #{target_type}: #{unmatched}" unless unmatched.empty?
                      enum_ann.named_args.each do |k, v|
                        spider << {target_type, [target_type, v], [{0, k, target_type}], :enum}
                      end
                    end
                  end
                  resolves << {j, target_name, target_type}
                  target_type.annotations(::Node::Aliased).each do |aliased|
                    j = aliased[0]
                    spider << {j, [j, target_type], [{0, "alias".id, target_type}], :alias}
                  end
                end
              end
              spider << {i, rule_args, resolves}
            end
          end
          i.annotations(::Node::Aliased).each do |aliased|
            j = aliased[0]
            spider << {j, [j, i], [{0, "alias".id, i}], :alias}
          end
        end
        spider = spider.uniq
      %}
      {% puts spider.map { |i| "#{i[0]}:#{" (#{i[3]})".id if i[3]}\n  args: #{i[1]}\n  resolve: #{i[2].map { |j| "\n    [#{j[0]}] = #{j[1]} : #{j[2]}" }.join("").id}\n\n" }.join("") %}
      {% for x in spider %}
        {% i, args, resolve, meta = x %}
        {% is_alias = meta == :alias %}
        {% is_enum = meta == :enum %}
        {% is_array = i.is_a? TypeNode && i <= ::Array %}
        {% is_ref = i.is_a? TypeNode && i <= ::Ref %}
        {% is_optional = meta == :optional %}
        {% args = args.map_with_index { |j, ji|
          if j.is_a? Path
            j.names.last.underscore.stringify
          elsif j.is_a? TypeNode
            sj = if j <= ::Array
                   j.type_vars[0].name(generic_args: false).split("::").last.underscore + "s"
                 else
                   j.name(generic_args: false).split("::").last.underscore
                 end
            sj = "opt_#{sj.id}" if is_optional && ji == 0
            sj
          else
            j
          end
        } %}
        builder.add({{ args.splat }}) do |context|
          {% if !is_enum %}
            {% for r in resolve %}
              {% r_idx, r_name, r_type = r %}
              {% r_type = r_type.type_vars[0] if r_type <= ::Ref %}
              {% opt_deref = r_type.union_types.includes?(::Nil) %}
              {% r_type = "Opt(#{r_type.union_types.find(&.!= ::Nil)})".id if opt_deref %}
              _r{{r_idx}} = context[{{ r_idx }}].value?({{ r_type }}) || raise ResolveFailureError.new("{{r_idx}} ({{ r_name }}){% if !is_array && !is_alias %} of {{ i }}{% end %}: expected {{ r_type }} but found #{context[{{ r_idx }}].stored_type_name}")
            {% end %}
          {% end %}
          {% if is_array && resolve.size == 2 %}
            _r0 << {% if args.size == 3 %} _r1 {% else %} _r2 {% end %}
            Any.new _r0
          {% elsif is_array && resolve.size == 1 %}
            _arr = {{ i }}.new
            _arr << _r0
            Any.new _arr
          {% elsif is_alias %}
            Any.new _r0
          {% elsif is_optional && resolve.size == 1 %}
            Any.new Opt({{ i }}).new _r0
          {% elsif is_optional && resolve.size == 0 %}
            Any.new Opt({{ i }}).new
          {% elsif is_enum %}
            {% r = resolve[0] %}
            Any.new {{ r[2] }}.new({{ r[1].symbolize }})
          {% else %}
            Any.new {{ i }}.new(
              {% for r in resolve %}{{ r[1] }}: {% if r[2] <= ::Ref %}::Ref.new(_r{{r[0]}}){% elsif r[2].union_types.includes?(::Nil) %}_r{{r[0]}}.value{% else %}_r{{r[0]}}{% end %},
              {% end %}
            )
          {% end %}
        end
      {% end %}
    {% debug %}
    {% end %}
  end
end

module JIS2
  @[Node::EnumVal(
    integer: :tt_int
  )]
  @[Node::Aliased(Type)]
  enum PrimitiveType
    Integer
  end
  @[Node::EnumVal(
    given: :r_given,
    repeat: :r_repeat,
    until: :r_until
  )]
  enum BlockStatementKind
    Given
    Repeat
    Until
  end
  alias Type = PrimitiveType
  record TypeName, type : Type, name : String do
    include Node
    structure Type >> @type, :word >> @name
  end

  abstract class Statement
    include Node
  end
  abstract struct Expression
    include Node
  end
  abstract struct BlockStatement
    include Node
  end
  record ExpressionBlockStatement < BlockStatement, kind : BlockStatementKind, value : Expression do
    include Node
    structure BlockStatementKind >> @kind, Expression >> @value
  end

  record Declaration < Expression, type : TypeName, value : Ref(Expression) do
    include Node
    structure TypeName >> @type, :assign, Expression >> @value
  end
  record DecimalLiteral < Expression, value : Int64 do
    include Node
    structure :number >> @value
  end

  record Finally, statement : Statement do
    include Node
    structure :r_finally, Statement >> @statement
  end

  class Block < Statement
    include Node
    getter b_statements : Array(BlockStatement)
    getter body : Array(Statement)
    getter finally : Finally?

    structure BlockStatements >> @b_statements, :r_do, Statements >> @body, OptFinally >> @finally, :r_end

    def initialize(@b_statements, @body, @finally)
    end
  end

  class FunctionDefinition < Statement
    include Node
    getter return_type : Type
    getter name : String
    @[Separator(:pipe, name: DefinitionArgs)]
    getter args : Array(TypeName)
    getter body : Array(Statement)

    structure Type >> @return_type, :r_func, :word >> @name, :sq_l, DefinitionArgs >> @args, :sq_r, Statements >> @body, :r_end

    def initialize(@return_type, @name, @args, @body)
    end
  end

  @[Node::Aliased(Program)]
  class Module
    include Node
    getter name : String
    getter body : Array(Statement)

    structure :r_module, :string >> @name, Statements >> @body, :r_end

    def initialize(@name, @body)
    end
  end
end

parser = Parser::Analysis(Parser::LR1::Builder).new
Node.compose_known_rules parser
states = parser.build("program")
at = Parser::Automaton.new(states)

at << :r_module << tok(:string, "default")
  at << :tt_int << :r_func << tok(:word, "collatz") << :sq_l
    at << :tt_int << tok(:word, "num")
  at << :sq_r
    at << :r_given << :tt_int << tok(:word, "a") << :assign << tok(:number, 1i64) << :r_do << :r_end
  at << :r_end
at << :r_end
tree = at.run
PrettyPrint.format(tree.value(JIS2::Module), STDOUT, 119)
puts
