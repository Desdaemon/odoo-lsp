from odoo import models, fields

class SaleOrder(models.Model):
    _inherit = 'sale.order'

    sale_crm_name = fields.Char()

    def sale_crm_method(self):
        """Method added by sale_crm auto_install module."""
        return "sale_crm integration active"

class CrmLead(models.Model):
    _inherit = 'crm.lead'

    sale_crm_name = fields.Char()

    def sale_crm_method(self):
        """Method added by sale_crm auto_install module."""
        return "sale_crm integration active"
