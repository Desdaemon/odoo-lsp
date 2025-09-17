from odoo import models, fields


class Bread(models.Model):
    _name = 'bakery.bread'

    def _test(self):
        items = {item: item for item in self}
        #^type Dict([Model("bakery.bread"), Model("bakery.bread")])

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
             #^type Dict([PyBuiltin("str"), Model("ir.model.fields")])
