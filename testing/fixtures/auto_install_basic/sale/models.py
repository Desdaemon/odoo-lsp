from odoo import models, fields

class SaleOrder(models.Model):
    _name = 'sale.order'
    _description = 'Sale Order'

    name = fields.Char()

    def sale_method(self):
        # Test that sale_crm methods are available via auto_install
        self.
        #   ^complete name sale_method sale_crm_name sale_crm_method
