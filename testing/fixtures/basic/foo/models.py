class Foo(Model):
    _name = "foo"

    bar = fields.Char()
    foo_m2o = fields.Many2one("foo")
    #                          ^complete bar derived.bar foo foob
    foo_o2m = fields.One2many("foo")
    #                          ^complete bar derived.bar foo foob
    foo_m2m = fields.Many2many("foo", 'bah', related='foo_m2o.foo_m2o')
    #                           ^complete bar derived.bar foo foob

    def completions(self):
        #^type Method(Symbol<ModelEntry>("foo"), "completions")
        self.env["bar"]
        #         ^complete bar derived.bar foo foob
        self.env['bogus']
        #         ^diag `bogus` is not a valid model name
        self._where_calc([('bar', '=', '123')])
        #                   ^complete bar foo_m2m foo_m2o foo_o2m
        self.flush_model(['bar'])
        #                  ^complete bar foo_m2m foo_m2o foo_o2m
        self.mapped("bar")
        #            ^complete bar foo_m2m foo_m2o foo_o2m
        for foo in self:
            foo.
        #      ^complete bar completions diagnostics foo_m2m foo_m2o foo_o2m

    def diagnostics(self):
        self.foo
        #    ^diag Model `foo` has no property `foo`
        self.env["foo"].foo
        #               ^diag Model `foo` has no property `foo`
        self.mapped("foo")
        #            ^diag Model `foo` has no field `foo`
        self.env["fo"]
        #         ^diag `fo` is not a valid model name
        self.env.ref('bogus.bogus')
        #             ^diag No XML record with ID `bogus.bogus` found
        self._context, self.pool, self.env


# TODO: More diagnostics for depends and compute
class Foob(Model):
    _name = "foob"
    _inherit = "bar"
    #           ^complete bar derived.bar foo foob

    foo_id = fields.Many2one(comodel_name="foo")
    #                                      ^complete bar derived.bar foo foob
    barb = fields.Char('Barb', related='foo_id.')
    #                                          ^complete bar foo_m2m foo_m2o foo_o2m
    hoeh = fields.Char('Hoeh', compute="_wunderbar")
    #                                   ^diag Model `foob` has no method `_wunderbar`

    @api.depends("foo_id")
    #             ^complete barb foo_id hoeh
    @api.constrains("foo_id.wah")
    #                      ^diag Dotted access is not supported in this context
    @api.onchange("foo_id.wah")
    #                    ^diag Dotted access is not supported in this context
    def handler(self):
        self.create({
            "foo_id"
            #^complete barb foo_id hoeh
        })
        self.create({
            "foo_id": ...
            #^complete barb foo_id hoeh
        })
        self.
        #   ^complete barb foo_id handler hoeh missing_depends

    @api.depends("barb")
    def missing_depends(self):
        for record in self:
            record.barb = bool(record.foo_id)

class NonModel:
    __slots__ = ("foo", "bar")

    def __init__(self):
        ...
