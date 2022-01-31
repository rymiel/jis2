struct Any
  @@known_type_names = {} of Int32 => String
  @object : Void*
  @stored_type : Int32

  def stored_type_name : String
    @@known_type_names[@stored_type]
  end

  def initialize(obj : T) forall T
    @object = Box.box obj
    @stored_type = obj.crystal_type_id
    @@known_type_names[@stored_type] = obj.class.name
  end

  def value(t : T.class) : T forall T
    {% begin %}
    if t.crystal_instance_type_id == @stored_type
      return Box(T).unbox @object
    {% for i in T.all_subclasses %}
    elsif {{i}}.crystal_instance_type_id == @stored_type
      return Box({{ i }}).unbox @object
    {% end %}
    end
    {% end %}
    raise "This Any is holding an instance of #{stored_type_name}, not #{t}"
  end

  def value?(t : T.class) : T? forall T
    {% begin %}
    if t.crystal_instance_type_id == @stored_type
      return Box(T).unbox @object
    {% for i in T.all_subclasses %}
    elsif {{i}}.crystal_instance_type_id == @stored_type
      return Box({{ i }}).unbox @object
    {% end %}
    end
    {% end %}
    nil
  end

  def inspect(io : IO)
    io << "#<Any"
    io << "(" << stored_type_name << ":" << @stored_type << "):"
    @object.address.to_s(io, 16)
    io << ">"
  end
end
