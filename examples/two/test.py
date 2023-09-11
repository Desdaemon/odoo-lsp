self.env.ref('one.one')

class Foo(Model):
    pass

class Bar(Model):
    _name = 'bar'
    _description = 'asd'
    barr = Boolean()
    bark = Char()
    food = Many2one('quux')

class Baz(models.Model):
    _inherit = 'bar'
    foo = fields.Char()

class Quux(models.Model):
    _name = 'quux'
    _inherit = 'bar'

class Moo(models.Model):
    _name = 'moo'
    _inherit = 'quux'

    haha = fields.Many2one(comodel_name='moo')
    def foo(self):
        self.env['moo']
        request.render('generic_tax_report')
