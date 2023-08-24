((class_definition
  (argument_list
    [(identifier) @_Model
     (attribute (identifier) @_models (identifier) @_Model)])
  (block
    (expression_statement
      (assignment
        (identifier) @_name
        (string) @name)?
      (assignment
        (identifier) @_inherit
        (string) @inherit)?)))
  (#eq? @_models "models")
  (#match? @_Model "^(Transient)?Model$")
  (#eq? @_name "_name")
  (#eq? @_inherit "_inherit"))
