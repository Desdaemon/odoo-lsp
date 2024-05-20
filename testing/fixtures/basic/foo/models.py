class Foo(Model):
    _name = "foo"

    bar = fields.Char()

    def completions(self):
        self.env["bar"]
        #         ^complete bar derived.bar foo foob
        for foo in self:
            foo.
        #       ^complete bar

    def diagnostics(self):
        self.foo
        #    ^diag Model `foo` has no field `foo`
        self.env["foo"].foo
        #               ^diag Model `foo` has no field `foo`
        self.mapped("foo")
        #            ^diag Model `foo` has no field `foo`
        self.env["fo"]
        #         ^diag `fo` is not a valid model name

class Foob(Model):
    _name = "foob"

    foo_id = fields.Many2one("foo")
    barb = fields.Char(related='foo_id.bar')
    #                                  ^complete bar

class NonModel:
    __slots__ = ("foo", "bar")

    def __init__(self):
        ...
