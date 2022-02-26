require "../jis2"

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
