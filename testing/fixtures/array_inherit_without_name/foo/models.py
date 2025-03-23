class Foo(Model):
    _name = "foo"

    age = fields.Char()


class Bar(Model):
    """Validation for #39"""

    # _name = 'foo'     # verifies that it still works without this
    _inherit = ["foo"]

    bar = fields.Char(related="age")
    #                          ^complete age bar

    def main(self):
        self.mapped("age")
        #            ^complete age bar
        self.mapped("aged")
        #            ^diag Model `foo` has no field `aged`
