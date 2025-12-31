class Unicorn:
    pass

class BaseModel:
    def create(self, vals):
        return vals

AbstractModel = BaseModel

class Model(AbstractModel):
    pass
