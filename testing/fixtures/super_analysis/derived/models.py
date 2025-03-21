class Foo(Model):
    _inherit = 'foo'

    @api.something_that_doesnt_exist
    def work(self):
        super().work()
        #^type Super
        bar = super().work()
        #^type Model("bar")
        bar.barf
        #  ^complete barf
        return bar
        #      ^type Model("bar")
