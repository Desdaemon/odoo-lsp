from odoo import models, fields

class SaleOrder(models.Model):
    _name = 'sale.order'
    _description = 'Sale Order'

    name = fields.Char()

    def sale_method(self):
        self.sale_crm_method()
        #    ^diag Model `sale.order` has no property `sale_crm_method`. This might be because module 'sale_crm' is marked as auto_install but cannot be loaded due to missing dependencies: 'crm'
