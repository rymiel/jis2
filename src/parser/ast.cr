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

  class ResolveFailureError < TypeCastError
    def initialize(@message)
    end
  end

  macro rule(*t, given_target = nil)
    {% for i in t %}
      {% if i.is_a? Call %}
        @[::Parser::AST::RuleNode(obj: {{ i.receiver }}, target: {{ i.args[0] }})]
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
                rule_args << a_obj
                if a_target
                  target = i.instance_vars.find(&.name.== a_target.name[1..])
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
                    if separator_ann = target.annotation(::Parser::AST::Separator)
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
      {% puts spider.map { |i| "#{i[0]}:#{" (#{i[3]})".id if i[3]}\n  args: #{i[1]}\n  resolve: #{i[2].map { |j| "\n    [#{j[0]}] = #{j[1]} : #{j[2]}" }.join("").id}\n\n" }.join("") %}
      {% for x in spider %}
        {% i, args, resolve, meta = x %}
        {% is_alias = meta == :alias %}
        {% is_enum = meta == :enum %}
        {% is_array = i.is_a? TypeNode && i <= ::Array %}
        {% is_ref = i.is_a? TypeNode && i <= ::Parser::Ref %}
        {% is_optional = meta == :optional %}
        {% args = args.map_with_index { |j, ji|
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
        } %}
        builder.add({{ args.splat }}) do |context|
          {% if !is_enum %}
            {% for r in resolve %}
              {% r_idx, r_name, r_type = r %}
              {% r_type = r_type.type_vars[0] if r_type <= ::Parser::Ref %}
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
              {% for r in resolve %}{{ r[1] }}: {% if r[2] <= ::Parser::Ref %}::Parser::Ref.new(_r{{r[0]}}){% elsif r[2].union_types.includes?(::Nil) %}_r{{r[0]}}.value{% else %}_r{{r[0]}}{% end %},
              {% end %}
            )
          {% end %}
        end
      {% end %}
    {% debug %}
    {% end %}
  end
end
