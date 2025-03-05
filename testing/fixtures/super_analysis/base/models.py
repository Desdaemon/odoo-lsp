class Foo(Model):
    _name = 'foo'

    def work(self):
        return self.env['bar']


class Bar(Model):
    _name = 'bar'

    barf = Char()
