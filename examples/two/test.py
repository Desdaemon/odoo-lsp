self.env.ref('one.one')

class Foo(Model):
    pass

class Bar(Model):
    _name = 'bar'

class Baz(models.Model):
    _inherit = 'bar'

class Quux(models.Model):
    _name = 'quux'
    _inherit = 'bar'

    what = fields.Many2one(comodel_name='bar')

class Moo(models.Model):
    _inherit = ['bar', 'quux']