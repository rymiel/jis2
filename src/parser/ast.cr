require "./util"

module Parser::AST
  annotation RuleNode
  end
  annotation Aliased
  end
  annotation Separator
  end
  annotation EnumVal
  end

  property! pos : Hash(Symbol, Pos)

  class ResolveFailureError < TypeCastError
    def initialize(@message)
    end
  end

  macro smart_enum(e, h)
    @[::Parser::AST::EnumVal(
    {% for k, v in h %}
      {{k}}: {{v}},
    {% end %}
    )]
    enum {{e}}
    {% for k, v in h %}
      {{k.camelcase}}
    {% end %}
    end
  end

  macro rule(*t, given_target = nil)
    {% for i in t %}
      {% if i.is_a? Call %}
        {% if i.name == :">>" %}
          @[::Parser::AST::RuleNode(obj: {{ i.receiver }}, target: {{ i.args[0] }})]
        {% elsif i.name == :"=~" %}
          @[::Parser::AST::RuleNode(obj: {{ i.receiver }}, target: {{ i.args[0] }}, absolute: true)]
        {% else %}
          {% i.raise %}
        {% end %}
      {% else %}
        @[::Parser::AST::RuleNode(obj: {{ i }})]
      {% end %}
    {% end %}
    def self._structure%structure{t}
    end
  end

  def self.compose_known_rules(builder : ::Parser::Analysis(T)) forall T
    {% begin %}
      {%
        spider = [] of Nil
        ::Parser::AST.includers.each do |i|
          if i.abstract?
            i.subclasses.each do |j|
              if ::Parser::AST.includers.includes?(j)
                spider << {i, [i, j], [{0, "subclass".id, j}], :alias}
              end
            end
          end
          i.class.methods.each do |m|
            if m.name.starts_with? "_structure"
              rule_args = [i]
              resolves = [] of Nil
              m.annotations(::Parser::AST::RuleNode).each_with_index do |a, j|
                a_obj = a[:obj]
                a_target = a[:target]
                a_abs = a[:absolute]
                if a_obj.is_a? InstanceVar
                  matching_metavar = i.instance_vars.find(&.name.== a_obj.name[1..])
                  a_target = a_obj
                  a_obj = matching_metavar.type
                  if metavar_separator = matching_metavar.annotation(::Parser::AST::Separator)
                    if metavar_custom_name = metavar_separator[:name]
                      a_obj = metavar_custom_name
                    end
                  end
                end
                if a_abs
                  rule_args << a_obj.resolve
                else
                  rule_args << a_obj
                end
                if a_target
                  target = i.instance_vars.find(&.name.== a_target.name[1..])
                  target_type = a_abs ? a_obj.resolve : target.type
                  target_name = target.name
                  if target_type.union?
                    union_types = target_type.union_types
                    if union_types.includes? ::Nil
                      base_type = union_types.find(&.!= ::Nil)
                      spider << {base_type, [base_type, base_type], [
                        {0, "optional".id, base_type},
                      ], :optional}
                      spider << {base_type, [base_type], [] of Nil, :optional}
                    end
                  end
                  if target_type <= ::Array
                    member = target_type.type_vars[0]
                    if separator_ann = target.annotation(::Parser::AST::Separator)
                      separator = separator_ann[0]
                      unique_name = separator_ann[:name] || target_type
                      spider << {target_type, [unique_name], [] of Nil}
                      spider << {target_type, [unique_name, member], [
                        {0, "array appended element".id, member},
                      ]}
                      spider << {target_type, [unique_name, unique_name, separator, member], [
                        {0, "array base".id, target_type},
                        {2, "array appended element".id, member},
                      ]}
                    else
                      spider << {target_type, [target_type], [] of Nil}
                      spider << {target_type, [target_type, target_type, member], [
                        {0, "array base".id, target_type},
                        {1, "array appended element".id, member},
                      ]}
                    end
                  elsif target_type <= ::Enum
                    if enum_ann = target_type.annotation(::Parser::AST::EnumVal)
                      ann_keys = enum_ann.named_args.keys
                      unmatched = target_type.constants.map(&.underscore).reject { |h| ann_keys.includes? h }
                      target.raise "EnumVal annotation is missing values for the enum #{target_type}: #{unmatched}" unless unmatched.empty?
                      enum_ann.named_args.each do |k, v|
                        spider << {target_type, [target_type, v], [{0, k, target_type}], :enum}
                      end
                    end
                  end
                  resolves << {j, target_name, target_type}
                  target_type.annotations(::Parser::AST::Aliased).each do |aliased|
                    j = aliased[0]
                    spider << {j, [j, target_type], [{0, "alias".id, target_type}], :alias}
                  end
                end
              end
              spider << {i, rule_args, resolves}
            end
          end
          i.annotations(::Parser::AST::Aliased).each do |aliased|
            j = aliased[0]
            spider << {j, [j, i], [{0, "alias".id, i}], :alias}
          end
        end
        spider = spider.uniq
      %}
      {% for x in spider %}
        {%
          i, args, resolve, meta = x
          is_alias = meta == :alias
          is_enum = meta == :enum
          is_array = i.is_a? TypeNode && i <= ::Array
          is_ref = i.is_a? TypeNode && i <= ::Parser::Ref
          is_optional = meta == :optional
          args = args.map_with_index { |j, ji|
            if j.is_a? Path
              j.names.last.underscore.stringify
            elsif j.is_a? TypeNode
              sj = if j <= ::Array
                     j.type_vars[0].name(generic_args: false).split("::").last.underscore + "s"
                   elsif j <= ::Parser::Ref
                     j.type_vars[0].name(generic_args: false).split("::").last.underscore
                   elsif j.union_types.includes?(::Nil)
                     "opt_#{j.union_types.find(&.!= ::Nil).name(generic_args: false).split("::").last.underscore.id}"
                   else
                     j.name(generic_args: false).split("::").last.underscore
                   end
              sj = "opt_#{sj.id}" if is_optional && ji == 0
              sj
            else
              j
            end
          }
        %}
        builder.add({{ args.splat }}) do |context|
          {% if resolve.size == 0 %}
            _pos = nil
          {% elsif resolve.size == 1 %}
            _pos = context[0].pos
          {% else %}
            _pos = ::Parser::Pos.join(context.map(&.pos).compact)
          {% end %}
          {% i_type = i.resolve? %}
          {% i_obj = i_type && i_type < ::Parser::AST %}
          {% if i_obj %}
            _obj_pos = {} of Symbol => ::Parser::Pos
          {% end %}
          {% if !is_enum %}
            {% for r in resolve %}
              {%
                r_idx, r_name, r_type = r
                r_type = r_type.type_vars[0] if r_type <= ::Parser::Ref
                opt_deref = r_type.union_types.includes?(::Nil)
                r_type = "Opt(#{r_type.union_types.find(&.!= ::Nil)})".id if opt_deref
                failure_location = "#{r_idx} (#{r_name})"
                failure_location += " of #{i}" unless is_array || is_alias
              %}
              %t{r}, %pos{r} = context[{{ r_idx }}].as_tuple
              _r{{r_idx}} = %t{r}.value?({{ r_type }}) || raise ResolveFailureError.new(
                "#{{{ failure_location }}}: expected {{ r_type }} but found #{%t{r}.stored_type_name}"
              )
              {% if i_obj %}
                _obj_pos[{{ r_name.symbolize }}] = %pos{r} if %pos{r}
              {% end %}
            {% end %}
          {% end %}
          {% if is_array && resolve.size == 2 %}
            _r0 << {% if args.size == 3 %} _r1 {% else %} _r2 {% end %}
            _f = Any.new _r0
          {% elsif is_array && resolve.size == 1 %}
            _arr = {{ i }}.new
            _arr << _r0
            _f = Any.new _arr
          {% elsif is_alias %}
            _f = Any.new _r0
          {% elsif is_optional && resolve.size == 1 %}
            _f = Any.new Opt({{ i }}).new _r0
          {% elsif is_optional && resolve.size == 0 %}
            _f = Any.new Opt({{ i }}).new
          {% elsif is_enum %}
            {% r = resolve[0] %}
            _f = Any.new {{ r[2] }}.new({{ r[1].symbolize }})
          {% else %}
            _obj = {{ i }}.new(
              {% for r in resolve %}
                {{ r[1] }}: ({% if r[2] <= ::Parser::Ref %}
                  ::Parser::Ref.new(_r{{r[0]}})
                {% elsif r[2].union_types.includes?(::Nil) %}
                  _r{{r[0]}}.value
                {% else %}
                  _r{{r[0]}}
                {% end %}),
              {% end %}
            )
            {% if i_obj %}
            _obj.pos = _obj_pos
            {% end %}
            _f = Any.new _obj
          {% end %}
          ::Parser::StackSym.new _f, _pos
        end
      {% end %}
    {% end %}
  end
end
