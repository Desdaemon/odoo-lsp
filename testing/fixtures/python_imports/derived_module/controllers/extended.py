from odoo.addons.base_module.controllers.main import CustomController
#                                                    ^def

from odoo.addons.base_module.controllers.main import CustomController as MyBaseController
#                                                                        ^def

from odoo.addons.nonexistent_module.controllers.main import MissingController
#                                                           ^diag Cannot resolve import 'MissingController'

from odoo.addons.base_module.controllers.missing import AnotherMissingController
#                                                       ^diag Cannot resolve import 'AnotherMissingController'


class ExtendedController(CustomController):
    #                    ^def
    ...


class ExtendedControllerWithBase(MyBaseController):
    #                            ^def
    ...
