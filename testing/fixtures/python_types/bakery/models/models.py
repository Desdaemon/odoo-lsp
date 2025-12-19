from odoo import models, fields


class Bread(models.Model):
    _name = 'bakery.bread'

    def _test(self):
        items = {item: item for item in self}
        #^type Dict(Model("bakery.bread"), Model("bakery.bread"))

        foobar = {'a': self, 'b': 123}
        #^type DictBag([("a", Model("bakery.bread")), ("b", PyBuiltin("int"))])

        aaaa = foobar['a']
        #^type Model("bakery.bread")

        bbbb = foobar['b']
        #^type PyBuiltin("int")

        return foobar

    def identity(self, what):
        return {'c': what}

    def _test_return(self):
        foobar = self._test()
        aaaa = foobar['a']
        #^type Model("bakery.bread")
        bbbb = foobar['b']
        #^type PyBuiltin("int")

        baz = self.identity(self)
        cccc = baz['c']
        #^type Model("bakery.bread")

    def test_variable_append(self):
        foo = []
        #^type List(...)
        foo.append(self)
        foo
        #^type List(Model("bakery.bread"))

    def test_dictkey_append(self):
        foo = self.identity([])
        foo['c'].append(self)
        cccc = foo['c']
        #^type List(Model("bakery.bread"))
        elem = cccc[12]
        #^type Model("bakery.bread")

    def test_dict_set(self):
        foobar = {}
        foobar['a'] = self
        aaaa = foobar['a']
        #^type Model("bakery.bread")
        foobar['b'] = nonexistent
        foobar
        #^type DictBag([("a", Model("bakery.bread")), ("b", Value)])

    def test_dict_update(self):
        foobar = {}
        foobar.update({'a': self})
        aaaa = foobar['a']
        #^type Model("bakery.bread")

    def test_sanity(self):
        foobar = ['what']
        #^type List(PyBuiltin("str"))
        self._fields
             #^type Dict(PyBuiltin("str"), Model("ir.model.fields"))

    def test_builtins(self):
        for aaaa, bbbb in enumerate(self):
            aaaa
            #^type PyBuiltin("int")
            bbbb
            #^type Model("bakery.bread")

        ints = [1, 2, 3]
        for aaaa, bbbb in zip(self, ints):
            aaaa
            #^type Model("bakery.bread")
            bbbb
            #^type PyBuiltin("int")
        
        what = [
            123 for
            cccc,
            #^type Model("bakery.bread")
            dddd
            #^type PyBuiltin("int")
            in zip(self, ints)
        ]

    def _identity_tuple(self, obj):
        return self, obj

    def _test_tuple(self):
        foo, bar = self._identity_tuple(123)
        #^type Model("bakery.bread")
        bar
        #^type PyBuiltin("int")

    def test_subscript(self):
        foobar = {'abcde': 123, 'fool': 234}
        foobar['']
        #      ^complete abcde fool
        foobar['f']
        #        ^complete fool


class Wine(models.Model):
    _name = 'bakery.wine'

    name = fields.Char()
    make = fields.Char()
    value = fields.Float()

    def _test_read_group(self):
        domain = []
        for name, make, value in self._read_group(domain, ['name', 'make'], ['value:sum']):
            #^type PyBuiltin("str")
            make
            #^type PyBuiltin("str")
            value
            #^type PyBuiltin("float")

    def _test_mapped(self):
        foo = self.mapped('make')
        #^type List(PyBuiltin("str"))

    def test_grouped(self):
        for name, records in self.grouped('name').items():
            #^type PyBuiltin("str")
            records
            #^type Model("bakery.wine")
        for (some, thing), _records in self.grouped(lambda mov: (mov.name, mov.value)).items():
            #^type PyBuiltin("str")
            thing
            #^type PyBuiltin("float")
