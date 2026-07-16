class IrConfigParameter(Model):
    _name = "ir.config_parameter"

    def get_param(self, key, default=False):
        return default

    def set_param(self, key, value):
        pass


class ResConfigSettings(Model):
    _name = "res.config.settings"

    setting = fields.Boolean(config_parameter='foo.from_kwarg')
    #                                          ^def

    def frobnicate(self):
        self.env['ir.config_parameter'].sudo().set_param('foo.from_set_param', '1')
        self.env['ir.config_parameter'].get_param('foo.from_xml')
        #                                          ^def
        self.env['ir.config_parameter'].sudo().get_param('foo.')
        #                                                     ^complete foo.from_kwarg foo.from_set_param foo.from_xml
