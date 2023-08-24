((call
  [(attribute (attribute (_) (identifier) @_env) (identifier) @_ref)
   (attribute (identifier) @_env (identifier) @_ref)]
  (argument_list . (string) @xml_id))
 (#eq? @_env "env")
 (#eq? @_ref "ref"))

((subscript
  [(identifier) @_env
   (attribute (_) (identifier) @_env)]
  (string) @model)
 (#eq? @_env "env"))

((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @_inherit
        (string) @model))))
 (#eq? @_inherit "_inherit"))