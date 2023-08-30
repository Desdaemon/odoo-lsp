((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @field
        (call
          [(identifier) @Type
           (attribute (identifier) @_fields (identifier) @Type)]
          (argument_list
            . (string)? @comodel_name
            (keyword_argument
              (identifier) @_arg
              [(string) (concatenated_string)] @value)?))))))
 (#eq? @_fields "fields")
 (#match? @Type "^[A-Z]"))