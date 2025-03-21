class Foo(Model):
    _name = 'foo'

class Bar(Model):
    _name = 'bar'

    foo_id = Many2one(comodel_name='foo')
    #                               ^complete bar foo
    bar_id = Many2one(comodel_name='bogus')
    #                               ^diag `bogus` is not a valid model name
