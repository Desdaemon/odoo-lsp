class Bar(models.Model):
    _name = "bar"


class DerivedBar(models.Model):
    _name = "derived.bar"
    _inherit = "bar"

    def test(self):
        self.env['bar']
