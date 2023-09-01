((class_definition
  (block
    (expression_statement
      [(assignment (identifier) @_name (string) @name)
       (assignment
        (identifier) @_inherit
        [(string) @inherit
         (list ((string) @inherit ","?)*)])])
    (decorated_definition
      (function_definition
        (parameters . (identifier) @self) (block) @scope))?
    (function_definition
      (parameters . (identifier) @self) (block) @scope)?)) @class
 (#eq? @_name "_name")
 (#eq? @_inherit "_inherit"))
