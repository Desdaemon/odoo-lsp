((class_definition
  (block 
    (expression_statement
      (assignment
        (identifier) @field
        [(call
          [(identifier) @_Relational
           (attribute (identifier) @_fields (identifier) @_Relational)]
          [(argument_list . (string) @relation
            (keyword_argument (identifier) @_help [(string) (concatenated_string)] @help)?)
           (argument_list
            (keyword_argument (identifier) @_comodel_name (string) @relation)
            (keyword_argument (identifier) @_help [(string) (concatenated_string)] @help)?)])
         (call
          [(identifier) @_Type
           (attribute (identifier) @_fields (identifier) @_Type)]
          (argument_list
            (keyword_argument (identifier) @_help [(string) (concatenated_string)] @help)?))]))))
 (#eq? @_fields "fields")
 (#match? @_Relational "^(Many2one|One2many|Many2many)$")
 (#match? @_Type "^[A-Z]")
 (#not-match? @_Type "^(Many2one|One2many|Many2many)$")
 (#eq? @_comodel_name "comodel_name")
 (#eq? @_help "help"))
