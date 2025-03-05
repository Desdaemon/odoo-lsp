class Foo(Model):
    _inherit = 'foo'

    def work(self):
        super().work()
        #^type Super
        bar = super().work()
        #^type Model("bar")
        bar.
        #  ^complete barf
