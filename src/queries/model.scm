((class_definition
  (argument_list
    [(identifier) @_Model
     (attribute (identifier) @_models (identifier) @_Model)])
  (block
    (expression_statement
      [(assignment (identifier) @_name (string) @name)
       (assignment
        (identifier) @_inherit
        [(string) @inherit
         (list (string) @inherit)] @model)])))
 (#eq? @_models "models")
 (#match? @_Model "^(Transient|Abstract)?Model$")
 (#eq? @_name "_name")
 (#eq? @_inherit "_inherit"))
