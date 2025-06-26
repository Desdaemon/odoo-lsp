class first(Model):
    _name = "first"

    second_id = fields.Many2one("second")

    def action_test(self):
        self.mapped("second_id")
        #            ^complete second_id
        self.mapped("second_id.name")
        #                      ^complete name
        self.mapped("second_id.dont_exist")
        #                      ^diag Model `second` has no field `dont_exist`


class Second(Model):
    _name = "second"

    name = fields.Char()
