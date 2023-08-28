((class_definition
  (block 
    (expression_statement
      (assignment
        (identifier) @field
        [(call
          [(identifier) @_Relational
           (attribute (identifier) @_fields (identifier) @_Relational)]
          [(argument_list . (string) @relation)
           (argument_list
            (keyword_argument (identifier) @_comodel_name (string) @relation))])
         (call
          [(identifier) @_Type
           (attribute (identifier) @_fields (identifier) @_Type)])]))*))
 (#eq? @_fields "fields")
 (#match? @_Relational "^(Many2one|One2many|Many2many)$")
 (#match? @_Type "^[A-Z]")
 (#eq? @_comodel_name "comodel_name"))
