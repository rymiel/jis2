require "./parser"
require "./lexer"

module JIS2
  alias AST = Parser::AST
  alias Ref = Parser::Ref
  alias Opt = Parser::Opt

  @[AST::Aliased(Type)]
  AST.smart_enum(PrimitiveType, {
    integer: :tt_int,
    void:    :tt_void,
  })

  AST.smart_enum(BlockStatementKind, {
    given:  :given,
    repeat: :repeat,
    until:  :until,
  })

  AST.smart_enum(Operator, {
    equal:      :"=",
    plus:       :"+",
    times:      :"*",
    minus:      :"-",
    divide:     :"÷",
    int_divide: :"÷∇",
    modulo:     :"mod",
  })

  alias Type = PrimitiveType

  record TypeName, type : Type, name : String do
    include AST
    rule @type, :_word >> @name

    def inspect(io : IO)
      io << "JIS2::TypeName("
      @type.to_s io
      io << " "
      @name.to_s io
      io << ")"
    end
  end

  abstract class Statement
    include AST
  end

  abstract struct Expression
    include AST
  end

  abstract struct BlockStatement
    include AST
  end

  record ExpressionBlockStatement < BlockStatement, kind : BlockStatementKind, value : Expression do
    include AST
    rule @kind, @value
  end

  record Declaration < Expression, type : TypeName, value : Ref(Expression) do
    include AST
    rule @type, :":=", @value
  end

  record Assignment < Expression, name : String, value : Ref(Expression) do
    include AST
    rule :_word >> @name, :":=", @value
  end

  record DecimalLiteral < Expression, value : Int64 do
    include AST
    rule :_number >> @value
  end

  record VariableLiteral < Expression, name : String do
    include AST
    rule :_word >> @name
  end

  record LeftReference < Expression do
    include AST
    rule :"$"
  end

  class ExpressionStatement < Statement
    include AST
    getter expression : Expression

    rule @expression, :";"

    def initialize(@expression)
    end
  end

  class ReturnStatement < Statement
    include AST
    getter expression : Expression

    rule :return, @expression, :";"

    def initialize(@expression)
    end
  end

  struct Call < Expression
    include AST
    getter name : String
    @[Separator(:"|", name: CallArgs)]
    getter args : Array(Expression)
    getter operator : Operator?

    rule :_word >> @name, :"[", @args, :"]"
    rule ::JIS2::Operator =~ @name, :"[", @args, :"]"

    def initialize(@name, @args)
    end

    def initialize(name @operator : Operator, @args)
      @name = ""
    end
  end

  record Finally, statement : Statement do
    include AST
    rule :finally, @statement
  end

  class Block < Statement
    include AST
    getter b_statements : Array(BlockStatement)
    getter body : Array(Statement)
    getter finally : Finally?

    rule @b_statements, :do, @body, @finally, :end

    def initialize(@b_statements, @body, @finally)
    end
  end

  class IfStatement < Statement
    include AST
    getter condition : Expression
    getter body : Array(Statement)
    getter else_body : Array(Statement)?

    rule :if, @condition, :then, @body, :end
    rule :if, @condition, :then, @body, :else, Array(::JIS2::Statement) =~ @else_body, :end

    def initialize(@condition, @body)
    end

    def initialize(@condition, @body, @else_body)
    end
  end

  class FunctionDefinition < Statement
    include AST
    getter return_type : Type
    getter name : String
    @[Separator(:"|", name: DefinitionArgs)]
    getter args : Array(TypeName)
    getter body : Array(Statement)

    rule :func, :_word >> @name, :"[", @args, :"]", @return_type, @body, :end

    def initialize(@return_type, @name, @args, @body)
    end

    def diagnostic(io : IO)
      @name.to_s io
      io << " ["
      @args.each { |i| io << "#{i.type} #{i.name}" }
      io << "] "
      @return_type.to_s io
    end

    def diagnostic : String
      String.build do |s|
        diagnostic s
      end
    end

    def to_s(io : IO)
      io << "#<JIS2::FunctionDefinition("
      diagnostic io
      io << ")>"
    end
  end

  @[AST::Aliased(Program)]
  class Module
    include AST
    getter name : String
    getter body : Array(Statement)

    rule :module, :_string >> @name, @body, :end

    def initialize(@name, @body)
    end
  end
end

class JIS2::Assembler
  class Scope
    getter vars = Hash(String, Type).new
    getter name : String
    getter? is_args : Bool

    def initialize(@name, @is_args = false)
    end

    def declare(tn : TypeName)
      raise "Duplicate declaration for #{tn}" if @vars.has_key? tn.name
      @vars[tn.name] = tn.type
    end
  end

  @scopes = [Scope.new "Global"]
  @bodies = [""]
  @ctx : FunctionDefinition? = nil
  @locals = Array({Scope, String, Type}).new
  @uniq = Atomic(Int32).new 0
  @left_references = Deque(TypeName).new
  @clobbers = Set(Symbol).new

  def allocate_temporary_register : Symbol
    return :l unless @clobbers.includes? :l
    return :m unless @clobbers.includes? :m

    raise "Out of registers! #{@clobbers}"
  end

  def add_clobber(reg : Symbol)
    raise "Duplicate clobber for #{reg}" if @clobbers.includes? reg
    @clobbers << reg
  end

  def release_clobber(reg : Symbol)
    raise "No clobber was present for #{reg}" unless @clobbers.delete reg
  end

  def initialize(@module : Module)
    append label "ENTRYPOINT"
    asmout "BS", label fn: "main"
    asmout "SYS", "1", "@o"
    asmout "HALT"
  end

  def new_scope(name : String? = nil, is_args : Bool = false)
    name ||= "Unnamed scope #{@scopes.size}"
    puts "|> Begin new scope (#{name})"
    asmout "; Begin scope #{name}"
    @scopes << Scope.new name, is_args
  end

  def end_scope : Scope
    ended = @scopes.pop
    puts "<| End scope (#{ended.name}) #{ended.vars}"
    asmout "; Ended scope #{ended.name}"
    ended
  end

  def declare(tn : TypeName)
    scope = @scopes.last
    scope.declare tn
    @locals << {scope, tn.name, tn.type}
  end

  def cur_body : String
    @bodies.last
  end

  def asmstr(*s, comment : String? = nil) : String
    msg = s.map_with_index { |i, j| (j > 1 ? ",\t" : "\t") + i.to_s }.join("")
    msg += "\t; #{comment}" if comment
    msg
  end

  def asmout(*s, comment : String? = nil) : Nil
    append asmstr *s, comment: comment
  end

  def append(s : String) : Nil
    @bodies << (@bodies.pop + s).rchop("\n") + "\n"
  end

  def find_var(name : String) : {type: Type, scope: Scope}
    @scopes.reverse.each do |i|
      if found = i.vars[name]?
        return {type: found, scope: i}
      end
    end
    raise "Couldn't find variable with name #{name} in any active scope"
  end

  def to_type(i) : Type
    if i.is_a? DecimalLiteral
      PrimitiveType::Integer
    elsif i.is_a? VariableLiteral
      find_var(i.name)[:type]
    elsif i.is_a? LeftReference
      to_type @left_references.last
    elsif i.is_a? Call
      types = i.args.map { |i| to_type(i) }
      if types.size == 2 && types.all? &.integer?
        if i.name.blank?
          return PrimitiveType::Integer
        end
      end
      raise "Can't deduce the type of #{i}"
    elsif i.responds_to? :type
      t = i.type
      if t.is_a? TypeName
        t.type
      else
        t
      end
    else
      raise "Can't deduce the type of #{i}"
    end
  end

  def type_size(t : Type)
    if t.integer?
      2
    else
      raise "Can't get size of type #{t}"
    end
  end

  def local_offset(name : String)
    var = find_var name
    idx = @locals.index({var[:scope], name, var[:type]})
    if var[:scope].is_args?
      param_idx = var[:scope].vars.keys.index name
      front = var[:scope].vars.values[..(param_idx.not_nil!)]
      -(front.map { |i| type_size i }.sum + 2)
    else
      front = @locals[...(idx.not_nil!)]
      front.map { |i| type_size i[2] }.sum + 2
    end
  end

  def to_value(ex : Expression, dest = :k)
    asmout comment: ex.to_s
    case ex
    when DecimalLiteral
      asmout "LOAD", "@#{dest}", "##{ex.value}"
    when Call
      p! ex
      types = ex.args.map { |i| to_type(i) }
      p! types
      if ex.name.blank?
        primitive = ex.operator.not_nil!
        if types.size == 2 && types.all?(&.integer?)
          a, b = ex.args

          temp_reg = allocate_temporary_register
          to_value a, temp_reg
          add_clobber temp_reg

          to_value b

          instr = case primitive
                  when .equal?, .minus? then "SUB"
                  when .plus?           then "ADD"
                  when .modulo?         then "MOD"
                  when .times?          then "MUL"
                  when .int_divide?     then "DIV"
                  else                       raise "invalid call to primitive #{ex}"
                  end

          asmout instr, "@#{dest}", "@#{temp_reg}", "@k"
          release_clobber temp_reg
        else
          raise "unimplemented primitive call #{ex}"
        end
      else
        matching_fn = @module.body.select(JIS2::FunctionDefinition).find { |f| f.args.map(&.type) == types && f.name == ex.name }.not_nil!
        ex.args.reverse_each.with_index do |arg, i|
          to_value arg
          asmout "PUSH", "@k", comment: "Argument #{matching_fn.args[-(i + 1)].name} to #{ex.name}"
        end
        asmout "BS", label matching_fn
        args_to_destroy = ex.args.map { |i| type_size to_type i }.sum
        asmout "DESTROY", args_to_destroy.to_s
        asmout "MOVE", "@#{dest}", "@o"
      end
    when Declaration
      @left_references << ex.type
      declare(ex.type)
      to_value(ex.value.value, dest)
      @left_references.pop
      offset = local_offset ex.type.name
      asmout "STORE", "#{offset}(@x)", "@#{dest}", comment: "#{ex.type.name} #{ex.type.type}"
    when Assignment
      var = find_var ex.name
      @left_references << TypeName.new var[:type], ex.name
      to_value(ex.value.value, dest)
      @left_references.pop
      offset = local_offset ex.name
      asmout "STORE", "#{offset}(@x)", "@#{dest}", comment: "#{ex.name} #{var[:type]}"
    when VariableLiteral
      var = find_var ex.name
      offset = local_offset ex.name
      asmout "LOAD", "@#{dest}", "#{offset}(@x)", comment: "#{ex.name} #{var[:type]}"
    when LeftReference
      lref = @left_references.last
      offset = local_offset lref.name
      asmout "LOAD", "@#{dest}", "#{offset}(@x)", comment: "#{lref.name} #{lref.type}"
    else
      raise "Expression #{ex} cannot be converted to a value"
    end
  end

  def assemble_body(body : Array(Statement))
    body.each do |st|
      asmout comment: st.to_s
      case st
      when ReturnStatement
        to_value(st.expression)
        asmout "MOVE", "@o", "@k"
        asmout "B", label @ctx.not_nil!, ret: true
      when Block
        new_scope "Block 0x#{st.object_id.to_s 16}"
        at_exit = [] of String
        st.b_statements.each do |b|
          p! b
          case b
          in ExpressionBlockStatement
            case b.kind
            when .given? then to_value(b.value)
            when .until?
              l = local_label "until"
              append label(l + ".B")
              to_value(b.value)
              asmout "B.EQZ", "@k", label(l + ".E")
              at_exit << asmstr "B", label(l + ".B")
              at_exit << label(l + ".E")
            else raise "Unknown expression block statement kind #{b}"
            end
          in BlockStatement
            raise "Unknown block statement #{b}"
          end
        end
        assemble_body st.body
        at_exit.each { |i| append i }
        if finally = st.finally
          assemble_body [finally.statement]
        end
        end_scope
      when ExpressionStatement
        to_value(st.expression)
      when IfStatement
        l = local_label "if"
        to_value(st.condition)
        else_body = st.else_body
        asmout "B.NEQZ", "@k", label(l + (else_body.nil? ? ".end" : ".else"))
        assemble_body st.body
        asmout "B", label(l + ".end")
        if else_body
          append label(l + ".else")
          assemble_body else_body
        end
        append label(l + ".end")
      else
        raise "unknown statement #{st}"
      end
    end
  end

  def label(fn : FunctionDefinition, *, ret : Bool = false)
    label fn: fn.name, ret: ret
  end

  def label(*, fn : String, ret : Bool = false)
    label "#{ret ? ".rF" : "F"}.#{fn}"
  end

  def label(s : String)
    "[#{s}]"
  end

  def local_label(description : String? = nil)
    ".l.#{description}#{description.nil? ? "" : "."}#{@uniq.add 1}"
  end

  def assemble(fn : FunctionDefinition)
    puts "\nAssembling #{fn}\n==="
    new_scope "Args of #{fn.diagnostic}", true
    fn.args.each_with_index do |i, j|
      declare(i)
    end
    new_scope "Locals of #{fn.diagnostic}"
    @locals.clear
    @ctx = fn
    @bodies << ""
    assemble_body(fn.body)
    end_scope

    fn_body = @bodies.pop
    @ctx = nil
    append label fn
    asmout "ENTER", @locals.map { |i| type_size i[2] }.sum
    append fn_body
    append label fn, ret: true
    asmout "LEAVE"
    asmout "RET"

    end_scope
  end
end

class JIS2::StyleWalker
  @last = 0

  def initialize(@input : String, @out : String::Builder)
  end

  def add(sym : Symbol, r = nil, *, pos : Parser::Pos)
    w = @input[pos.s...pos.e]
    @out << @input[@last...pos.s]
    @last = pos.e
    color = if sym.in?(:module, :func, :if, :then, :else, :do, :end, :given, :until, :repeat, :finally, :return)
              Colorize::Color256.new 172
            elsif sym.in?(:_string, :_number)
              Colorize::ColorANSI::Magenta
            elsif sym == :tt_int
              Colorize::ColorANSI::Green
            elsif sym.in?(:":=", :"=", :+, :-, :*, :÷∇, :÷, :mod)
              Colorize::Color256.new 147
            elsif sym.in?(:|, :";")
              Colorize::ColorANSI::LightGray
            else
              Colorize::ColorANSI::White
            end
    @out << w.colorize color
  end

  def eof
  end
end

lexer = Lexer(String, Int64).build do
  match :module, :func, :if, :then, :else, :do, :end, :given, :until, :repeat, :finally, :return
  match /∇/, :tt_int
  match :"[", :"]", :":=", :"=", :|, :"$", :";", :+, :-, :*, :÷∇, :÷, :mod
  match /"([^"]*)"/, :_string, &.[1]
  match /!(\d+)/, :_number, &.[1].to_i64
  match /[a-z]+/, :_word, &.[0]
  skip /\s+/
end

input = <<-JIS2
module "default"
  func collatz[∇ n] ∇
    given ∇ count := !0
    until =[n | !1] do
      if =[mod[n | !2] | !0] then
        n := ÷∇[$ | !2];
      else
        n := +[*[$ | !3] | !1];
      end
      count := +[$ | !1];
    finally return count;
    end
  end

  func main[] ∇
    return collatz[!7];
  end
end
JIS2

style_out = String.build do |sb|
  style = JIS2::StyleWalker.new input, sb
  lexer.lex(input, style)
end
puts style_out

parser = Parser::Analysis(Parser::LR1::Builder).new
puts "Composing rules"
JIS2::AST.compose_known_rules parser
puts "Building parser"
rules = parser.build("program")
puts "Assembling automaton"
at = Parser::Automaton.new(rules)

puts "Running lexer"
top_level = lexer.lex(input, at).not_nil!.t.value(JIS2::Module)
puts "Complete!"

PrettyPrint.format(top_level, STDOUT, 119)
puts

assembler = JIS2::Assembler.new(top_level)
top_level.body.select(JIS2::FunctionDefinition).each do |fn|
  assembler.assemble fn
end
result_asm = assembler.@bodies.pop
puts result_asm
File.open(ARGV[0], "w") do |f|
  f << result_asm
end
