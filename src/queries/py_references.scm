((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @_field
        (string) @model))))
  (#match? @_field "^_(name|inherit)$"))
