from odoo import http


class CustomController(http.Controller):
    def helper_method(self):
        return "helper"
