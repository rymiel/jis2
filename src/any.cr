struct Any
  @object : Void*
  @stored_type : Int32
  @stored_type_name : String
  @to_s_proc : Proc(IO, Nil)
  @inspect_proc : Proc(IO, Nil)

  def initialize(obj)
    @object = Box.box obj
    @stored_type = obj.class.crystal_type_id
    @stored_type_name = obj.class.to_s
    @to_s_proc = ->obj.to_s(IO)
    @inspect_proc = ->obj.inspect(IO)
  end

  def value(t : T.class) : T forall T
    if t.crystal_type_id != @stored_type
      raise "This Any is holding an instance of #{@stored_type_name}, not #{t}"
    end
    return Box(T).unbox @object
  end

  def to_s(io : IO)
    @to_s_proc.call(io)
  end

  def inspect(io : IO)
    io << "#<Any"
    # object_id.to_s(io, 16)
    io << "(" << @stored_type_name << ":" << @stored_type << "):"
    @object.address.to_s(io, 16)
    io << " "
    @inspect_proc.call(io)
    io << ">"
  end
end
