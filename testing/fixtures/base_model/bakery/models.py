class Cake(Model):
    _name = "cake"

    flavor = fields.Char()

    def browsed(self):
        records = self.browse([1])
        records.
        #      ^complete _where_calc browse browsed create ensure_one exists filtered flavor flush_model grouped mapped read search searched sudo sudoed unlink write

    def sudoed(self):
        records = self.sudo()
        records.
        #      ^complete _where_calc browse browsed create ensure_one exists filtered flavor flush_model grouped mapped read search searched sudo sudoed unlink write

    def searched(self):
        records = self.search([])
        records.
        #      ^complete _where_calc browse browsed create ensure_one exists filtered flavor flush_model grouped mapped read search searched sudo sudoed unlink write


class Pie(Model):
    _name = "pie"
    _inherit = "cake"

    crust = fields.Char()

    def search(self, domain, offset=0, limit=None, order=None):
        return super().search(domain, offset=offset, limit=limit, order=order)

    def overridden(self):
        records = self.search([])
        records.
        #      ^complete _where_calc browse browsed create crust ensure_one exists filtered flavor flush_model from_env grouped mapped overridden read search searched sudo sudoed unlink write

    def from_env(self):
        records = self.env["cake"].search([])
        records.
        #      ^complete _where_calc browse browsed create ensure_one exists filtered flavor flush_model grouped mapped read search searched sudo sudoed unlink write
