class Main(models.Model):
    _name = 'main'

    def test_expression(self):
        foo = self if True else self
        #^type Model("main")
        bar = self if True else 123  # we just pick an arbitrary one
        #^type Model("main")
