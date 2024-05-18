self.env.ref("one.one")


class Foo(Model):
    pass


class Bar(Model):
    _name = "bar"
    _description = "asd"
    barr = Boolean()
    bark = Char()
    food = Many2one("quux")


class Baz(models.Model):
    _inherit = "moo"

    def what():
        pass


class Quux(models.Model):
    _name = "quux"
    _inherit = "bar"


class Moo(models.Model):
    _name = "moo"
    _inherit = "quux"

    hahar = fields.Many2one(comodel_name="moo")

    def foo(self):
        self.env["moo"].hahar
        request.render("generic_tax_report")
