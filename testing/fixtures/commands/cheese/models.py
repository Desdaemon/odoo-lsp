from odoo import Commands


class Cheese(models.Model):
    _name = 'cheese'

    name = fields.Char()


class CheeseBox(models.Model):
    _name = 'cheese.box'

    cheese_ids = fields.One2many('cheese')
    child_ids = fields.One2many('cheese.box')
    parent_id = fields.Many2one('cheese.box')

    def cheesey(self):
        self.create([{
            'cheese_ids': [(0, 0, {
                'name': ...
                #^complete name
            })],
            'child_ids': [(0, 0, {
                # ^complete child_ids
                'cheese_ids': [Command.create({
                    ''
                    #^complete name
                })]
            })]
        }])