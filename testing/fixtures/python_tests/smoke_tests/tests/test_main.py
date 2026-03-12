class SomeTestCase:
    def setUpClass(cls):
        cls.foobar = cls.env['what']
        #   ^type Model("what")
    
    def test_sanity(self):
        self.foobar
        #    ^type Model("what")
