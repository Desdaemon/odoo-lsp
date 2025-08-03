from odoo import models, fields

class SaleOrder(models.Model):
    _name = 'sale.order'
    _description = 'Sale Order'

    name = fields.Char()

    def sale_method(self):
        self.sale_crm_method()
        #    ^diag Model `sale.order` has no property `sale_crm_method`
        #    ^related This property is defined in `sale_crm`
        #    ^related To expose this property, depend directly on `sale_crm` or all of its reverse dependencies
        #    ^related `sale_crm` is defined as a bridge module here, alongside its reverse dependencies
