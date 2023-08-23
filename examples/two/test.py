self.env.ref('asd')

class Foo(Model):
    pass

class Bar(Model):
    _name = 'bar'

class Baz(Model):
    _inherit = 'bar'

class Quux(Model):
    _name = 'quux'
    _inherit = 'bar'