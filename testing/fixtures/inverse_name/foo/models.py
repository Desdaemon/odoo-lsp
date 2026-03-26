class Foo(models.Model):
    _name = 'foo'

    foo_id = fields.Many2one('foo')
    bar_id = fields.Many2one('bar')
    unrelated = fields.Char()
    foo_ids = fields.One2many('foo', inverse_name='foo')
    #                                              ^complete foo_id

class Bar(models.Model):
    _name = 'bar'

    foo_id = fields.One2many('foo', inverse_name='bar_id')
    #                                             ^complete bar_id
