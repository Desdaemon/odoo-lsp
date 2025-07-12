from odoo import models, fields, api


class TestModel(models.Model):
    _name = "test.model"
    _description = "Test Model"

    name = fields.Char()

    def test_method(self):
        return True

    def another_method(self, param):
        return param
