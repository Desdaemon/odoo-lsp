class Foo(Model):
    _name = 'foo'

    some_field = Char()


class FooController:
    @http.route('/do_stuff')
    def do_stuff(self):
        foo = request.env['foo']
        #^type Model("foo")
        foo.
        #  ^complete some_field
