from odoo import models, fields

class CrmLead(models.Model):
    _name = 'crm.lead'
    _description = 'CRM Lead'

    name = fields.Char()

    def crm_method(self):
        # Test that sale_crm methods are available via auto_install
        self.
        #   ^complete name crm_method sale_crm_name sale_crm_method
