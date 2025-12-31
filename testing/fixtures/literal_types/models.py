"""Test literal type inference for Python strings, ints, floats, bools, lists, dicts"""

from odoo import models, fields


class TestLiterals(models.Model):
    _name = "test.literals"
    _description = "Test Literal Type Inference"

    # String literals should infer as str type
    test_string = fields.Char(
        string="Test String",
        default="hello world",  # str literal
        help="String with default",
    )

    test_number = fields.Integer(
        default=42,  # int literal  
        help="Integer with default",
    )

    test_float = fields.Float(
        default=3.14,  # float literal
    )

    # Test in method context
    def method_with_literals(self):
        """Test literal type inference in method body"""
        # String literal
        msg = "Hello, World!"
        #^type PyBuiltin("str")
        # Integer literal
        count = 100
               #^type PyBuiltin("int")
        # Float literal
        ratio = 0.5
               #^type PyBuiltin("float")
        # Boolean literals
        is_active = True
                   #^type PyBuiltin("bool")
        is_deleted = False
                   #^type PyBuiltin("bool")
        # None literal
        empty_value = None
                    #^type PyBuiltin("None")
        
        # List with literals - should infer as List[int]
        numbers = [1, 2, 3, 4, 5]
                 #^type List(PyBuiltin("int"))
        # List with strings
        strings = ["a", "b", "c"]
                #^type List(PyBuiltin("str"))
        
        # Dict with literals - should infer dict with str key and mixed values
        config = {
                #^type DictBag([("name", PyBuiltin("str")), ("count", PyBuiltin("int")), ("enabled", PyBuiltin("bool"))])
            "name": "test",  # str key and value
            "count": 42,  # int value
            "enabled": True,  # bool value
        }
        
        return {
            "message": msg,
            "count": count,
            "ratio": ratio,
            "is_active": is_active,
            "is_deleted": is_deleted,
            "empty": empty_value,
            "numbers": numbers,
            "strings": strings,
            "config": config,
        }

    def tuple_test(self):
        """Test tuple literal type inference"""
        simple_tuple = (1, 2, 3)
                     #^type Tuple([PyBuiltin("int"), PyBuiltin("int"), PyBuiltin("int")])
        mixed_tuple = ("a", 1, 3.14, True)
                    #^type Tuple([PyBuiltin("str"), PyBuiltin("int"), PyBuiltin("float"), PyBuiltin("bool")])
        nested_tuple = ("x", (1, 2), "y")
                     #^type Tuple([PyBuiltin("str"), Tuple([PyBuiltin("int"), PyBuiltin("int")]), PyBuiltin("str")])
        return simple_tuple, mixed_tuple, nested_tuple
