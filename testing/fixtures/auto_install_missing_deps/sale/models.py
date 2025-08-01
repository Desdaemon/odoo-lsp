from odoo import models, fields

class SaleOrder(models.Model):
    _name = 'sale.order'
    _description = 'Sale Order'

    name = fields.Char()

    def sale_method(self):
        self.sale_crm_method()
        #    ^diag Model `sale.order` has no property `sale_crm_method`
