from odoo.addons.base_module.controllers.main import CustomController
#                                                    ^def

from odoo.addons.base_module.controllers.main import CustomController as MyBaseController
#                                                                        ^def

from odoo.addons.nonexistent_module.controllers.main import MissingController
#                                                           ^diag Cannot resolve import 'odoo.addons.nonexistent_module.controllers.main'

from odoo.addons.base_module.controllers.missing import AnotherMissingController
#                                                       ^diag Cannot resolve import 'odoo.addons.base_module.controllers.missing'


class ExtendedController(CustomController):
    #                    ^def
    ...


class ExtendedControllerWithBase(MyBaseController):
    #                            ^def
    ...
