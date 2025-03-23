class Foo(Model):
    _name = 'foo'

    def foody(self, obj):
        return obj

    def bar(self):
        obj = self.foody(self)
        # TODO ^ type Model("foo")
