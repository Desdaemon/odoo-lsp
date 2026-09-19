class BaseModel(metaclass=MetaModel):
    __slots__ = ['env', '_ids', '_prefetch_ids']

    def browse(self, ids=()) -> Self:
        return self._browse(self.env, ids, ids)

    def sudo(self, flag=True) -> Self:
        return self.with_env(self.env(su=flag))

    @api.returns('self')
    def search(self, domain, offset=0, limit=None, order=None):
        return self._search(domain)

    def exists(self) -> Self:
        return self

    def ensure_one(self) -> Self:
        return self

    def filtered(self, func) -> Self:
        return self

    def create(self, vals_list) -> Self:
        return self

    def write(self, vals) -> bool:
        return True

    def unlink(self) -> bool:
        return True

    def mapped(self, func):
        return self

    def grouped(self, key):
        return {}

    def read(self, fields=None, load='_classic_read'):
        return []

    def flush_model(self, fnames=None):
        return None

    def _where_calc(self, domain, active_test=True):
        return None
