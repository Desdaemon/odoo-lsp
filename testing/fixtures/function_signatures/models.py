"""Test function signature extraction and parameter type tracking"""

from odoo import models, fields, api


class TestFunctionSignatures(models.Model):
    _name = "test.function_signatures"
    _description = "Test Function Signature Extraction"

    name = fields.Char(string="Name")
    dependent_field = fields.Char(string="Dependent Field")
    computed = fields.Char(string="Computed Field", compute="compute_field")

    # Test simple function with parameters
    def simple_function(self, name, count):
        """Test simple function parameters"""
        return name, count

    def method_with_return_annotation(self) -> dict:
        """Test method with return type annotation"""
        return {}

    def method_with_param_types(self, value: int, text: str) -> bool:
        """Test method with both param and return type annotations"""
        return len(text) == value

    # Test varargs and kwargs
    def varargs_method(self, *args, **kwargs):
        """Test function with *args and **kwargs"""
        return args, kwargs

    def called_with_params(self):
        """Test calling functions with type checking"""
        # These calls should validate parameter types
        result = self.simple_function("test", 42)
        #^type Tuple([PyBuiltin("str"), PyBuiltin("int")])
        
        result2 = self.method_with_return_annotation()
        #^type DictBag([])
        
        result3 = self.method_with_param_types(10, "hello")
        #^type PyBuiltin("bool")
        
        return result, result2, result3

    @api.model
    def create_model(self):
        """Test model creation and field defaults"""
        return self.create({
            "name": "test",  # Should be str
        })

    @api.depends("dependent_field")
    def compute_field(self):
        """Test computed field with dependencies"""
        for record in self:
            record.computed = record.dependent_field or ""
