((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @field
        (call
          [(identifier) @Type
           (attribute (identifier) @_fields (identifier) @Type)]
          (argument_list . (string)? @relation
            ((keyword_argument (identifier) @_arg (_) @value) ","?)*))))))
 (#eq? @_fields "fields")
 (#match? @Type "^[A-Z]"))