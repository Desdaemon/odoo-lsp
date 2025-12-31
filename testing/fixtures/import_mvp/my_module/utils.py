"""Module with various Python definitions to import."""

# Simple variable
DEFAULT_TIMEOUT = 30

# Simple function
def calculate_total(items, tax_rate=0.1):
    """Calculate total with tax."""
    subtotal = sum(items)
    return subtotal * (1 + tax_rate)


# Class definition
class User:
    """User class."""
    def __init__(self, name, email):
        self.name = name
        self.email = email
    
    def get_display_name(self):
        return f"{self.name} <{self.email}>"


# Another class
class Customer(User):
    """Customer class that extends User."""
    def __init__(self, name, email, customer_id):
        super().__init__(name, email)
        self.customer_id = customer_id
    
    def get_id(self):
        return self.customer_id


# Constants
MAX_RETRIES = 3
API_VERSION = "v2"
