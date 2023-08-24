self.env.ref('one.view_one')

class Foo(Model):
    pass

class Bar(Model):
    _name = 'bar'

class Baz(models.Model):
    _inherit = 'bar'

class Quux(models.Model):
    _name = 'quux'
    _inherit = 'bar'