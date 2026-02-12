from odoo import models
from odoo.models import Unicorn, BaseModel
#    ^def 
#         ^def 
#                       ^def 
#                                ^def
from odoo import Command
#                ^def
from odoo.tools import floaty
#                      ^def

class What(models.Model):
    #             ^def
    _name = 'what'
    def _test_base(self):
        self.create({})
        #         ^complete create
