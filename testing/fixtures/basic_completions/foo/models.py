class FooLine(Model):
    _name = "foo.line"

    parent_id = fields.Many2one("foo")
    name = fields.Char()
    quantity = fields.Float()
    price = fields.Float()
    total = fields.Float()
    notes = fields.Text()

class Foo(Model):
    _name = "foo"

    name = fields.Char()
    line_ids = fields.One2many("foo.line", "parent_id")

    def test_command_list_broken_syntax(self):
        """Test various broken syntax scenarios in command lists"""

        self.create({
            'name': 'Test Record',
            'line_ids': [(0, 0, {
                'name': 'Line 1',
                'quan'
                #  ^complete name notes parent_id price quantity total
            })]
        })

        self.create({
            'line_ids': [(0, 0, {
                'name': 'Line 2',
                'pr'
                # ^complete name notes parent_id price quantity total
            })]
        })

        vals = {
            'name': 'Parent',
            'line_ids': [(0, 0, {
                'name': 'Nested',
                'not'
                # ^complete name notes parent_id price quantity total
            })]
        }

        self.env['foo'].create({
            'line_ids': [(0, 0, {
                'na'
                # ^complete name notes
            }), (0, 0, {
                'quan': 5,
                'pri'
                #   ^complete name notes parent_id price quantity total
            })]
        })

        self.create({
            'name': 'Deep Test',
            'line_ids': [(0, 0, {
                'name': 'Line',
                'parent_id': self.id,
                'total'
                #    ^complete name notes parent_id price quantity total
            })]
        })
