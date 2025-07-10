class Foo(Model):
    _name = "foo"

    bar = fields.Char(groups="bar.group_name")
    #                              ^complete bar.group_name

    def action_button(self):
        ...

    def action_button2(self):
        ...
