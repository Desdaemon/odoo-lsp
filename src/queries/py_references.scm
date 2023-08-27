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
        (identifier) @_field
        [(string) @model
         (list (string) @model)]))))
 (#match? @_field "^_(name|inherit)$"))

((call
  [(identifier) @_Field
   (attribute (identifier) @_fields (identifier) @_Field)]
  [(argument_list . (string) @model)
   (argument_list
    (keyword_argument (identifier) @_comodel_name (string) @model))])
 (#eq? @_fields "fields")
 (#eq? @_comodel_name "comodel_name")
 (#match? @_Field "^(Many2one|One2many|Many2many)$"))
