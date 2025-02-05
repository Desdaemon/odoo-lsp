class Foo(Model):
    _name = 'foo'
    age = fields.Integer()


class Bar(Model):
    _name = 'bar'
    foo_id = fields.Many2one('foo', domain=[('age', '>', 42)])
    #                                         ^complete age
    foo_related_id = fields.Many2one(comodel_name='foo', domain=[('age', '<', 100)])
    #                                                              ^complete age
