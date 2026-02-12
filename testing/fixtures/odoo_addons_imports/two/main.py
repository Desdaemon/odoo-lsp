from odoo.addons.one.models.partner import ResPartner, DEFAULT_LIMIT
#                                          ^def
#                                                      ^def

partner = ResPartner()
#         ^def
total = partner.compute_total()
