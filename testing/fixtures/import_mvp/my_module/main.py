"""Test various import patterns and symbol resolution."""

# Test 1: Import specific symbols
from .utils import User, calculate_total, DEFAULT_TIMEOUT
#                  ^def
#                        ^def
#                                         ^def

# Test 2: Import submodule (from . import foo pattern)
from . import logging
#             ^def

# Test 3: Use imported class
def create_user(name, email):
    return User(name, email)
    #      ^def

# Test 4: Use imported function
def process_order(items):
    total = calculate_total(items)
    #       ^def
    return total

# Test 5: Use imported constant
def get_timeout():
    return DEFAULT_TIMEOUT
    #      ^def

# Test 6: Use imported submodule
logger = logging.Logger()
#        ^def

# Test 7: Import class and use it
from .utils import Customer
#                  ^def

def create_customer(name, email, cid):
    c = Customer(name, email, cid)
    #   ^def
    return c

# Test 8: Import with alias
from .utils import MAX_RETRIES as max_attempts
#                  ^def

def retry_count():
    return max_attempts
    #      ^def
