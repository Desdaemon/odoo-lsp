class Foo(Model):
    _name = "foo"

    def test(self):
        self.env["bar"]

    def diagnostics(self):
        self.foo
        self.env["foo"].foo
        self.mapped("foo")
        self.env["fo"]


class Foob(Model):
    _name = "foob"
